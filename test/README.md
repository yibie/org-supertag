# Virtual Column Test Guide

## Quick Start

### Step 1: Load the module (fresh Emacs session)
```elisp
(add-to-list 'load-path "/Users/chenyibin/Documents/emacs/package/org-supertag")
(load-file "/Users/chenyibin/Documents/emacs/package/org-supertag/supertag-virtual-column.el")
```

### Step 2: Run quick test
```elisp
(load-file "/Users/chenyibin/Documents/emacs/package/org-supertag/test/quick-test.el")
```

Expected output:
```
=== Virtual Column Quick Test ===
Test 1: Creating virtual column...
✓ Create: PASS
Test 2: Getting definition...
✓ Get: PASS
Test 3: Cache operations...
✓ Cache: PASS
Test 4: List columns...
✓ List: PASS (1 column)
Test 5: Delete column...
✓ Delete: PASS
=== Quick Test Complete ===
```

### Step 3: Run interactive demo
```elisp
(load-file "/Users/chenyibin/Documents/emacs/package/org-supertag/test/demo-virtual-column.el")
M-x supertag-demo-virtual-column
```

### Step 4: Run full ERT tests
```elisp
(load-file "/Users/chenyibin/Documents/emacs/package/org-supertag/test/virtual-column-test.el")
M-x ert-run-tests-interactively
```

## Troubleshooting

### Error: `(void-variable total-effort)`
**Cause**: Old version of module with quoted plist in docstring  
**Fix**: Re-load the updated `supertag-virtual-column.el` file

### Error: `Module not loaded`
**Cause**: `load-path` not set correctly  
**Fix**: Ensure path includes the org-supertag directory

### Error: `Feature not found`
**Cause**: Module failed to load due to dependencies  
**Fix**: Load dependencies first:
```elisp
(require 'supertag-core-store)
(require 'supertag-core-schema)
(require 'supertag-virtual-column)
```
