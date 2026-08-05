# Widget Renderer 实机验收

## 1. 启动隔离图形 Emacs

```sh
open -na /Applications/Emacs.app --args -Q \
  --eval '(package-initialize)' \
  -L /Users/chenyibin/Documents/emacs/package/org-supertag \
  -l supertag-view-framework.el \
  -l supertag-view-progress-dashboard.el \
  -l supertag-view-effort-distribution.el \
  -l supertag-view-priority-matrix.el
```

确认这是独立 `-Q` 实例，不影响日常 Emacs。可分别运行 `M-x supertag-view-progress-dashboard-demo`、`M-x supertag-view-effort-distribution-demo`、`M-x supertag-view-priority-matrix-demo`，确认三个 View 能打开、`M-x supertag-view-refresh` 后仍在同一 buffer，标题和正文没有消失。

## 2. 建立交互夹具

在 `*scratch*` 粘贴并执行 `M-x eval-buffer`：

```elisp
(supertag-view-define-from-config
 '(:id widget-hands-on
   :name "Widget Hands-on"
   :persist nil
   :widgets
   ((:type :text :key intro :content "Outside text is read-only")
    (:type :columns
     :columns
     ((:width 18
       :children
       ((:type :button :key run :label "Run"
         :action (lambda () (message "Run activated")))))
      (:width 18
       :children
       ((:type :link :key tag :label "#emacs/package"
         :action (lambda () (message "Tag activated")))))))
    (:type :card :title "Editor" :width 18
     :children
     ((:type :editable-field :key title
       :value "旧值" :width 10
       :on-change (lambda (value) (message "Value: %s" value))))))))

(supertag-view-open 'widget-hands-on '(:tag "demo" :nodes nil))
```

## 3. 可见与交互检查

1. `Run`、`#emacs/package` 和 `旧值` 均位于对齐的 columns/card 边框内；中文字段没有撑歪右边框。
2. `TAB` / `S-TAB` 依次到达 link、button、field；在 `Run` 或 link 上按 `RET`，echo area 显示对应消息。
3. 在 field 内修改文字，echo area 显示新值；在 field 外直接键入普通字符应被拒绝。
4. 将 point 放在 `Run` 中间，执行 `M-x supertag-view-refresh`；point 仍回到 `Run`，按钮与字段仍可用，边框不漂移。
5. 重复 refresh 三次，确认没有重复控件、残留高亮或越来越多的空行。

## 4. 批准门禁

通过后回复“Widget Renderer 实机通过，可以提交”。在此之前 task007 保持未完成，不 commit/push。
