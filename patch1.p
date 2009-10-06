---
 lib/app2.scm       |   11 ++++-------
 lib/ffi/iphone.scm |    3 +--
 lib/ffi/osx.scm    |    6 ++----
 3 files changed, 7 insertions(+), 13 deletions(-)

diff --git a/lib/app2.scm b/lib/app2.scm
index b7c855c..759af16 100644
--- a/lib/app2.scm
+++ b/lib/app2.scm
@@ -39,13 +39,10 @@
 (define %%screen-height #f)

 (define (select-view)
-  ;; This part of the ffi crashes right now
-  ;; (let* ((bounds (UIView-bounds (current-view)))
-  ;;        (size (CGRect-size bounds)))
-  ;;   (set! %%screen-width (CGSize-width size))
-  ;;   (set! %%screen-height (CGSize-height size)))
-  (set! %%screen-width (UIView-width (current-view)))
-  (set! %%screen-height (UIView-height (current-view))))
+  (let* ((bounds (UIView-bounds (current-view)))
+         (size (CGRect-size bounds)))
+    (set! %%screen-width (CGSize-width size))
+    (set! %%screen-height (CGSize-height size))))

 (define (view-space-x n)
  (exact->inexact (/ n %%screen-width)))
diff --git a/lib/ffi/iphone.scm b/lib/ffi/iphone.scm
index 97754d1..6315bdd 100644
--- a/lib/ffi/iphone.scm
+++ b/lib/ffi/iphone.scm
@@ -13,10 +13,9 @@
 (c-define (register-view view) (UIView*) void "register_view" ""
  (current-view view))

-;;; WARNING: Do not use this function yet, it crashes for some reason
 (define UIView-bounds
  (c-lambda (UIView*) CGRect #<<end-c-code
-   CGRect *res = malloc(sizeof(CGRect));
+   CGRect *res = ___EXT(___alloc_rc(sizeof(CGRect)));
   *res = ___arg1.bounds;
   ___result_voidstar = res;
 end-c-code
diff --git a/lib/ffi/osx.scm b/lib/ffi/osx.scm
index 3402dbe..80d8854 100644
--- a/lib/ffi/osx.scm
+++ b/lib/ffi/osx.scm
@@ -78,19 +78,17 @@ end-c-code

 ;;;;; CGRect

-;;; WARNING: Do not use this function yet, it crashes for some reason
 (define CGRect-origin
  (c-lambda (CGRect) CGPoint #<<end-c-code
-   CGPoint *res = malloc(sizeof(CGPoint));
+   CGPoint *res = ___EXT(___alloc_rc(sizeof(CGPoint)));
   *res = ___arg1.origin;
   ___result_voidstar = res;
 end-c-code
 ))

-;;; WARNING: Do not use this function yet, it crashes for some reason
 (define CGRect-size
  (c-lambda (CGRect) CGSize #<<end-c-code
-   CGSize *res = malloc(sizeof(CGSize));
+   CGSize *res = ___EXT(___alloc_rc(sizeof(CGSize)));
   *res = ___arg1.size;
   ___result_voidstar = res;
 end-c-code
--
