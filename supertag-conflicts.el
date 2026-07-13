;;; supertag-conflicts.el --- Review and resolve semantic sync conflicts -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; P1-5 of .phrase/phases/phase-git-sync-20260713/PLAN.md's review list:
;; `supertag-merge.el' (S3) already records true semantic merge conflicts
;; into a `:sync-conflicts' store collection -- see its Commentary section
;; "Conflict representation" for the exact record shape and id scheme this
;; file reads -- but until now nothing let a user SEE or RESOLVE them. This
;; file closes that loop, per the plan's "真冲突的呈现" ("How true conflicts
;; are surfaced") normative requirement (PLAN.md around line 163): load-time
;; message + doctor section (see supertag-doctor.el's "9. Sync Conflicts")
;; + an interactive per-key resolve command / manual edit.
;;
;; ## Record shape (frozen by supertag-merge.el; summarized here for
;; reference, not redefined):
;;
;;   (:id ID :collection C :entity-id ID-OR-NIL :key K-OR-NIL
;;    :ours V1 :theirs V2 :base V0 :kind :field-conflict|:delete-vs-modify
;;    :detected-at nil)
;;
;; C/ENTITY-ID/KEY vary by conflict shape:
;;   - C nil, ENTITY-ID nil, KEY a keyword: a root scalar conflict (e.g.
;;     `:version') -- applies directly to a top-level key of `supertag--store'.
;;   - C a normal collection (`:nodes' etc.), ENTITY-ID set, KEY nil: a
;;     whole-entity conflict (`:kind' is still `:field-conflict' for this
;;     shape, OR `:delete-vs-modify' when one side deleted the entity) --
;;     V1/V2 are the entity's entire data (or the absent-placeholder below).
;;   - C a normal collection, ENTITY-ID set, KEY a keyword equal to
;;     `supertag--persistence--hash-marker' (`:supertag-hash-table'): a
;;     `:field-values'/legacy-`:fields' entity is stored as a raw hash
;;     table, which freezes to a plist with exactly that one pseudo-key --
;;     so this is ALSO whole-entity-value granularity in disguise (see
;;     supertag-merge.el's Commentary "Conflict granularity" point 2).
;;   - C a normal collection, ENTITY-ID set, KEY any other keyword: an
;;     ordinary field-level conflict on that entity's plist.
;;   - C `:tag-field-associations', ENTITY-ID a tag-id, KEY a string
;;     field-id: one association slot's whole plist conflicted (`:kind' is
;;     `:field-conflict' or `:delete-vs-modify', the latter when one side
;;     dropped that field-id from the tag).
;;   - C `:tag-field-associations', ENTITY-ID a tag-id, KEY `:order': the
;;     ordered-set's LIST ORDER conflicted -- V1/V2/V0 are ordered lists of
;;     field-id STRINGS, not association plists.
;;
;; A deleted side (in a `:delete-vs-modify' record, at either whole-entity
;; or tag-field-associations-slot granularity) is recorded as the keyword
;; `:supertag-merge/absent' (`supertag-merge--absent-placeholder' in
;; supertag-merge.el). This file intentionally does NOT `require' that
;; module to get the constant: supertag-merge.el is designed as an
;; independent batch/merge-driver tool (it even calls `package-initialize'
;; at load time, guarded, for that standalone use) that nothing else in the
;; runtime requires; pulling it into the normal Emacs startup path just for
;; one literal keyword would be exactly the kind of load-order coupling
;; this feature must avoid. The keyword itself is duplicated instead (see
;; `supertag-conflicts--absent-placeholder' below), documented so the two
;; can't silently drift without a human noticing.
;;
;; ## Load-time visibility (hook choice, justified)
;;
;; supertag-core-persistence.el must not `require' this file. Rather than
;; the alternative (a fboundp-guarded check bolted onto org-supertag.el's
;; `supertag-init', which only covers ONE of the several places
;; `supertag-load-store' is actually called from -- see also
;; `supertag-vault-activate') this file instead hooks onto
;; `supertag-persistence-after-load-hook', a normal hook that
;; supertag-core-persistence.el runs (via `run-hooks', not knowing or
;; caring who is listening) at the tail of every SUCCESSFUL
;; `supertag-load-store' call. This is the exact same idiom already
;; established in this codebase for this exact kind of problem:
;; `supertag-git.el' hooks `supertag-git-sync--on-db-saved' onto the
;; symmetric `supertag-persistence-after-save-hook' the same way, and
;; supertag-core-persistence.el is none the wiser about either listener.
;; The only requirement this places on load order is that THIS file be
;; `require'd (directly or transitively) before the first
;; `supertag-load-store' call happens to run -- true for the normal
;; `org-supertag.el' startup path (which requires this file alongside its
;; other core-tier modules), and something any other caller (a test, a
;; batch script) opts into explicitly by requiring this file too.
;;
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'supertag-core-store)
(require 'supertag-core-transform) ; For `supertag-with-transaction'
(require 'supertag-core-persistence) ; For save/dirty/thaw/hash-marker + the after-load hook

;;; --- Constants ---

(defconst supertag-conflicts--absent-placeholder :supertag-merge/absent
  "Mirrors `supertag-merge--absent-placeholder' in supertag-merge.el.
See the top-level Commentary for why this one literal keyword is
duplicated here rather than `require'-ing that module.")

(defconst supertag-conflicts--missing (make-symbol "supertag-conflicts-missing")
  "Internal sentinel distinguishing \"key absent\" from \"key present, value nil\"
in raw `gethash' existence checks below.")

;;; --- Query helpers ---

(defun supertag-conflicts-list ()
  "Return every recorded sync conflict as a list of plists.
Each plist is one `:sync-conflicts' entity, per the record shape
documented in this file's Commentary (and, normatively, in
supertag-merge.el's). Returns the empty list when the `:sync-conflicts'
collection does not exist yet or is empty -- never errors, and never
creates the collection merely by being asked. Sorted by `:id' so output
(and completion-candidate order) is stable across calls."
  (if (not (hash-table-p supertag--store))
      nil
    (let ((bucket (gethash :sync-conflicts supertag--store)))
      (if (not (hash-table-p bucket))
          nil
        (let (result)
          (maphash (lambda (_id data) (push data result)) bucket)
          (sort result (lambda (a b) (string< (plist-get a :id) (plist-get b :id)))))))))

(defun supertag-conflicts-count ()
  "Return the number of recorded sync conflicts."
  (length (supertag-conflicts-list)))

;;; --- Target existence ---

(defun supertag-conflicts--target-exists-p (collection entity-id)
  "Return non-nil if (COLLECTION . ENTITY-ID) currently exists live.
COLLECTION nil (a root-scalar conflict) always \"exists\": the top-level
store itself is always present once initialized, so root conflicts never
hit the \"target deleted since the merge\" path."
  (if (null collection)
      t
    (let ((bucket (supertag-store-get-collection collection)))
      (not (eq (gethash entity-id bucket supertag-conflicts--missing)
               supertag-conflicts--missing)))))

;;; --- Applying a resolved value to its target (record removal is the
;;; caller's job -- see `supertag-conflicts--resolve-one') ---

(defun supertag-conflicts--apply-assoc-slot (tag-id field-id value)
  "Upsert or drop FIELD-ID's association plist in TAG-ID's ordered
`:tag-field-associations' list, based on resolved VALUE: the
absent-placeholder means \"drop this field-id\"; anything else is the
whole replacement association plist for it (inserted at the end when
FIELD-ID was not previously present)."
  (let* ((current (supertag-store-get-tag-field-associations tag-id))
         (test (lambda (a) (equal (plist-get a :field-id) field-id)))
         (present (cl-find-if test current)))
    (supertag-store-put-tag-field-associations
     tag-id
     (cond
      ((eq value supertag-conflicts--absent-placeholder)
       (cl-remove-if test current))
      (present
       (mapcar (lambda (a) (if (funcall test a) value a)) current))
      (t (append current (list value))))
     t)))

(defun supertag-conflicts--apply-assoc-order (tag-id order)
  "Reorder TAG-ID's `:tag-field-associations' list per ORDER (an ordered
list of field-id strings recorded at merge time). Field-ids named in ORDER
that are still present are moved to the front in that relative sequence;
anything currently present but not named in ORDER (e.g. added after the
merge) is appended afterward, preserving its current relative order --
never silently dropped just because it postdates the conflict record."
  (let* ((current (supertag-store-get-tag-field-associations tag-id))
         (index (make-hash-table :test 'equal)))
    (dolist (a current) (puthash (plist-get a :field-id) a index))
    (let* ((ordered (delq nil (mapcar (lambda (fid) (gethash fid index)) order)))
           (seen (make-hash-table :test 'equal)))
      (dolist (a ordered) (puthash (plist-get a :field-id) t seen))
      (let ((rest (cl-remove-if (lambda (a) (gethash (plist-get a :field-id) seen)) current)))
        (supertag-store-put-tag-field-associations tag-id (append ordered rest) t)))))

(defun supertag-conflicts--apply (collection entity-id key value)
  "Apply resolved VALUE to the live store for one non-root conflict.
Dispatches on COLLECTION/KEY per the shapes documented in this file's
Commentary. Does not touch the `:sync-conflicts' record itself -- see
`supertag-conflicts--resolve-one', which wraps this together with the
record removal in one transaction. NOTE: does not handle the
`supertag--persistence--hash-marker' key (see
`supertag-conflicts--resolve-hash-entity-one', which the caller dispatches
to for that key instead, before ever reaching this function) nor a root
conflict (COLLECTION nil, see `supertag-conflicts--resolve-root-one') --
both need their own atomicity strategy, documented at those functions."
  (cond
   ((and (eq collection :tag-field-associations) (eq key :order))
    (supertag-conflicts--apply-assoc-order entity-id value))
   ((eq collection :tag-field-associations)
    (supertag-conflicts--apply-assoc-slot entity-id key value))
   ((null key)
    ;; Whole-entity replace/delete. Every CURRENT collection's whole-entity
    ;; conflicts carry a plist (or another non-hash-table value -- see the
    ;; safety net in supertag-merge.el's `supertag-merge--decompose-conflict')
    ;; here, never a raw hash table (a hash-table-shaped entity always
    ;; freezes to the ONE-key `:supertag-hash-table' pseudo-plist, which is
    ;; caught by KEY instead, at the field-level granularity dispatched
    ;; before this function is even called -- see
    ;; `supertag-conflicts--resolve-one'), so `supertag-store-put-entity'
    ;; (whose `supertag--normalize-entity' step flattens any hash-table
    ;; VALUE into a plist) is safe to use unconditionally here. If some
    ;; future collection ever produces a genuinely hash-table-shaped
    ;; whole-entity (KEY nil) conflict, it would need the same manual
    ;; untracked-write treatment as
    ;; `supertag-conflicts--resolve-hash-entity-one' below.
    (if (eq value supertag-conflicts--absent-placeholder)
        (supertag-store-remove-entity collection entity-id)
      (supertag-store-put-entity collection entity-id
                                  (supertag--persistence--thaw-value value) t)))
   (t
    (let ((current (copy-sequence (supertag-store-get-entity collection entity-id))))
      (supertag-store-put-entity
       collection entity-id
       (plist-put current key (supertag--persistence--thaw-value value))
       t)))))

;;; --- Manual-atomicity targets: root scalars and raw-hash-table entities ---
;;
;; Two conflict shapes cannot go through the tracked collection/entity
;; transaction primitives at all:
;;
;;   - Root scalars (e.g. `:version') live directly as top-level keys of
;;     `supertag--store', not inside a collection sub-table.
;;   - `:field-values'/legacy-`:fields' entities are genuinely raw hash
;;     tables in the live store (see `supertag-store-put-field-value').
;;     Routing a resolved value for one of these through
;;     `supertag-store-put-entity' would silently corrupt it:
;;     `supertag--normalize-entity' (called by that function's
;;     `supertag-store--put-and-notify' seam) unconditionally FLATTENS any
;;     hash-table VALUE into a plist -- exactly correct for the ordinary
;;     plist-entity collections it was designed for, but wrong here.
;;
;; Neither shape is understood by the shared `supertag-with-transaction'
;; rollback log either way (`supertag--transaction-restore-entry' in
;; supertag-core-transform.el only knows collection/entity and field-value
;; paths) -- a raw `puthash' bypassing the tracked primitives would not be
;; rolled back on error. `supertag-conflicts--resolve-manual' gets the same
;; net atomicity guarantee by hand instead: it captures the pre-resolution
;; value itself, lets the conflict-record removal run inside its own
;; (tracked) transaction alongside the untracked mutation, and manually
;; restores the captured value if that transaction's body signals an
;; error, then re-signals so the caller still sees the failure.

(defun supertag-conflicts--resolve-manual (id effective get-old set-old mutate)
  "Shared manual-atomicity driver. GET-OLD (0-arg) captures the
pre-resolution value; SET-OLD (1-arg) restores it verbatim on error;
MUTATE (0-arg) performs the resolved write and is only called when
EFFECTIVE is not `:drop'. Returns `:applied' or `:dropped'."
  (let ((old (funcall get-old)))
    (condition-case err
        (progn
          (supertag-with-transaction
            (if (eq effective :drop)
                (supertag-store-remove-entity :sync-conflicts id)
              (progn (funcall mutate)
                     (supertag-store-remove-entity :sync-conflicts id))))
          (if (eq effective :drop) :dropped :applied))
      (error
       (funcall set-old old)
       (signal (car err) (cdr err))))))

(defun supertag-conflicts--resolve-root-one (conflict id effective value)
  "Root-scalar-conflict path for `supertag-conflicts--resolve-one'."
  (let ((key (plist-get conflict :key)))
    (supertag-conflicts--resolve-manual
     id effective
     (lambda ()
       (cons (not (eq (gethash key supertag--store supertag-conflicts--missing)
                       supertag-conflicts--missing))
             (gethash key supertag--store)))
     (lambda (old)
       (if (car old) (puthash key (cdr old) supertag--store) (remhash key supertag--store)))
     (lambda ()
       (let ((chosen (pcase effective
                       (:use-ours (plist-get conflict :ours))
                       (:use-theirs (plist-get conflict :theirs))
                       (:edit value))))
         (puthash key chosen supertag--store)
         (supertag-mark-dirty))))))

(defun supertag-conflicts--resolve-hash-entity-one (conflict id collection entity-id effective value)
  "Raw-hash-table-entity conflict path for `supertag-conflicts--resolve-one'
(KEY is `supertag--persistence--hash-marker' -- see this section's
Commentary). VALUE (when EFFECTIVE is `:edit') and the conflict's
`:ours'/`:theirs' are each the raw alist recorded at merge time (see
supertag-merge.el's Commentary) -- re-wrapped in the marker shape
`supertag--persistence--thaw-value' recognizes before use."
  (supertag-conflicts--resolve-manual
   id effective
   (lambda ()
     (let ((bucket (supertag-store-get-collection collection)))
       (cons (not (eq (gethash entity-id bucket supertag-conflicts--missing)
                       supertag-conflicts--missing))
             (gethash entity-id bucket))))
   (lambda (old)
     (let ((bucket (supertag-store-get-collection collection)))
       (if (car old) (puthash entity-id (cdr old) bucket) (remhash entity-id bucket))))
   (lambda ()
     (let* ((chosen (pcase effective
                      (:use-ours (plist-get conflict :ours))
                      (:use-theirs (plist-get conflict :theirs))
                      (:edit value)))
            (thawed (supertag--persistence--thaw-value
                     (list supertag--persistence--hash-marker chosen)))
            (bucket (supertag-store-get-collection collection)))
       (puthash entity-id thawed bucket)
       (supertag-mark-dirty)
       (supertag-emit-event :store-changed (list collection entity-id) nil thawed)))))

;;; --- Single-conflict resolution (the atomic primitive) ---

(defun supertag-conflicts--resolve-one (id action &optional value)
  "Resolve the `:sync-conflicts' entity ID via ACTION.
ACTION is one of `:use-ours', `:use-theirs', `:edit', or `:drop'. VALUE
supplies the replacement when ACTION is `:edit' (ignored otherwise).

Atomic per call: applying the resolved value to its target entity AND
removing the conflict record happen inside one `supertag-with-transaction'
-- an error part-way through rolls back both, leaving the target entity
and the conflict record exactly as they were.

When the conflict's target no longer exists (its owning entity was
deleted since the merge -- e.g. the user deleted the node), silently
downgrades to `:drop' regardless of the requested ACTION: there is
nothing left to apply the value to, so the only sane resolution is
dropping the stale record.

Returns `:applied', `:dropped', or `:not-found' (ID was already gone from
`:sync-conflicts', e.g. resolved by a concurrent call)."
  (let ((conflict (supertag-store-get-entity :sync-conflicts id)))
    (if (null conflict)
        :not-found
      (let* ((collection (plist-get conflict :collection))
             (entity-id (plist-get conflict :entity-id))
             (key (plist-get conflict :key))
             (target-exists (supertag-conflicts--target-exists-p collection entity-id))
             (effective (if (and (not (eq action :drop)) (not target-exists)) :drop action)))
        (cond
         ((null collection)
          (supertag-conflicts--resolve-root-one conflict id effective value))
         ((eq key supertag--persistence--hash-marker)
          (supertag-conflicts--resolve-hash-entity-one
           conflict id collection entity-id effective value))
         (t
          (supertag-with-transaction
            (if (eq effective :drop)
                (supertag-store-remove-entity :sync-conflicts id)
              (let ((chosen (pcase effective
                              (:use-ours (plist-get conflict :ours))
                              (:use-theirs (plist-get conflict :theirs))
                              (:edit value))))
                (supertag-conflicts--apply collection entity-id key chosen)
                (supertag-store-remove-entity :sync-conflicts id))))
          (if (eq effective :drop) :dropped :applied)))))))

;;; --- Save integration ---

(defun supertag-conflicts--save-and-report ()
  "Call `supertag-save-store' and report (via `message') whether it
actually persisted, respecting whatever guards `supertag-save-store'
itself enforces (this call is never `called-interactively-p', so a guard
violation degrades to `supertag-save-store''s own message rather than
raising `user-error')."
  (if (not (fboundp 'supertag-save-store))
      (message "Supertag: conflict resolution applied in memory (supertag-save-store not available).")
    (supertag-save-store)
    (if (and (fboundp 'supertag-dirty-p) (supertag-dirty-p))
        (message "Supertag: conflict resolution applied in memory, but the save was skipped just now (see the message above for why) -- run M-x supertag-doctor or M-x supertag-save-store once resolved.")
      (message "Supertag: conflict resolution saved to disk."))))

;;; --- Presentation helpers (shared by the interactive commands) ---

(defun supertag-conflicts--describe-value (v)
  "Compact, truncated `format' rendering of one conflict-side value V."
  (let ((s (if (eq v supertag-conflicts--absent-placeholder) "<deleted>" (format "%S" v))))
    (if (> (length s) 60) (concat (substring s 0 57) "...") s)))

(defun supertag-conflicts--label (conflict)
  "Return one `completing-read' candidate label for CONFLICT."
  (format "%s: ours=%s theirs=%s"
          (plist-get conflict :id)
          (supertag-conflicts--describe-value (plist-get conflict :ours))
          (supertag-conflicts--describe-value (plist-get conflict :theirs))))

(defun supertag-conflicts--read-edit-value (conflict)
  "Prompt for a replacement value for CONFLICT, choosing the reader by the
shape of its recorded value: `read-string' for a string, `read-number' for
a number, or a full sexp reader (`read' over `read-string''s input) for
anything else -- plists, association plists, order lists, etc."
  (let* ((sample (plist-get conflict :ours))
         (sample (if (eq sample supertag-conflicts--absent-placeholder)
                     (plist-get conflict :theirs)
                   sample)))
    (cond
     ((stringp sample) (read-string "New value (string): " sample))
     ((numberp sample) (read-number "New value (number): " sample))
     (t (read (read-string "New value (sexp): " (format "%S" sample)))))))

;;; --- Interactive commands ---

;;;###autoload
(defun supertag-conflicts-resolve ()
  "Interactively resolve one recorded sync conflict.
Prompts (via `completing-read') for which conflict to resolve, annotated
with both sides' (truncated) values, then offers use-ours / use-theirs /
edit-value / skip -- or, when that conflict's target entity no longer
exists, only the option to drop the stale conflict record. See this
file's Commentary, and supertag-merge.el's \"Conflict representation\",
for the record shape."
  (interactive)
  (let ((conflicts (supertag-conflicts-list)))
    (if (null conflicts)
        (message "Supertag: no sync conflicts.")
      (let* ((labels (mapcar #'supertag-conflicts--label conflicts))
             (choice (completing-read "Resolve sync conflict: " labels nil t))
             (idx (cl-position choice labels :test #'string=))
             (conflict (nth idx conflicts))
             (id (plist-get conflict :id))
             (exists (supertag-conflicts--target-exists-p
                      (plist-get conflict :collection) (plist-get conflict :entity-id))))
        (if (not exists)
            (if (y-or-n-p (format "Target for conflict %s no longer exists. Drop this conflict record? " id))
                (progn
                  (supertag-conflicts--resolve-one id :drop nil)
                  (supertag-conflicts--save-and-report)
                  (message "Supertag: dropped stale conflict record %s." id))
              (message "Supertag: left conflict %s unresolved." id))
          (let* ((action-label
                  (completing-read (format "Conflict %s -- action: " id)
                                    '("use-ours" "use-theirs" "edit-value" "skip") nil t))
                 (action (pcase action-label
                           ("use-ours" :use-ours)
                           ("use-theirs" :use-theirs)
                           ("edit-value" :edit)
                           (_ :skip))))
            (if (eq action :skip)
                (message "Supertag: skipped conflict %s." id)
              (let ((value (and (eq action :edit) (supertag-conflicts--read-edit-value conflict))))
                (supertag-conflicts--resolve-one id action value)
                (supertag-conflicts--save-and-report)
                (message "Supertag: resolved conflict %s (%s)." id action-label)))))))))

(defun supertag-conflicts--resolve-all (action label)
  "Shared driver for the bulk use-ours-all/use-theirs-all commands.
Resolves every current conflict with ACTION (falling back to `:drop' per
conflict, automatically, whenever that conflict's own target is gone --
see `supertag-conflicts--resolve-one'), tolerating and reporting
per-conflict failures rather than aborting the whole batch, then saves
once and reports."
  (let ((conflicts (supertag-conflicts-list)))
    (if (null conflicts)
        (message "Supertag: no sync conflicts.")
      (when (y-or-n-p (format "Resolve all %d sync conflict(s) with `%s'? "
                               (length conflicts) label))
        (let ((applied 0) (dropped 0) (failed 0))
          (dolist (c conflicts)
            (condition-case err
                (pcase (supertag-conflicts--resolve-one (plist-get c :id) action nil)
                  (:applied (cl-incf applied))
                  (:dropped (cl-incf dropped)))
              (error
               (cl-incf failed)
               (message "Supertag: failed to resolve conflict %s: %s"
                        (plist-get c :id) (error-message-string err)))))
          (supertag-conflicts--save-and-report)
          (message "Supertag: resolved %d of %d conflict(s) (%d applied, %d dropped)%s."
                   (+ applied dropped) (length conflicts) applied dropped
                   (if (> failed 0) (format "; %d FAILED and left unresolved" failed) "")))))))

;;;###autoload
(defun supertag-conflicts-use-ours-all ()
  "Resolve every recorded sync conflict by keeping the `ours' side.
Any conflict whose target entity no longer exists is resolved by dropping
its stale record instead (there is nothing to apply `ours' to)."
  (interactive)
  (supertag-conflicts--resolve-all :use-ours "use-ours"))

;;;###autoload
(defun supertag-conflicts-use-theirs-all ()
  "Resolve every recorded sync conflict by taking the `theirs' side.
Any conflict whose target entity no longer exists is resolved by dropping
its stale record instead (there is nothing to apply `theirs' to)."
  (interactive)
  (supertag-conflicts--resolve-all :use-theirs "use-theirs"))

;;; --- Load-time visibility ---

(defun supertag-conflicts--notify-after-load ()
  "Hook function for `supertag-persistence-after-load-hook': message once,
with a count and a pointer to `M-x supertag-conflicts-resolve', whenever
the just-loaded store carries recorded sync conflicts. See this file's
top-level Commentary, \"Load-time visibility\", for why this file
registers itself here instead of supertag-core-persistence.el knowing
about this file."
  (let ((count (supertag-conflicts-count)))
    (when (> count 0)
      (message "Supertag: %d sync conflict(s) need attention -- run M-x supertag-conflicts-resolve to review them."
               count))))

(add-hook 'supertag-persistence-after-load-hook #'supertag-conflicts--notify-after-load)

(provide 'supertag-conflicts)

;;; supertag-conflicts.el ends here
