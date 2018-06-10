(asdf:defsystem "cl-autorepo"
  :description "A simple mechanism to auto-load ASDF systems from repositories."
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "1.0.0"
  :license "Apache"
  :depends-on ()
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "autorepo")))))
