;;;; clpm.asd - ASDF system definition for CLPM

(defsystem "clpm"
  :version "0.1.0"
  :author "CLPM Authors"
  :license "MIT"
  :description "A technically excellent package manager for SBCL"
  :depends-on ()  ; No external dependencies for bootstrap
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "errors")
     (:module "io"
      :serial t
      :components
      ((:file "sexp")
       (:file "fs")))
     (:module "crypto"
      :serial t
      :components
      ((:file "sha256")
       (:file "ed25519")))
     (:file "platform")
     (:file "store")
     (:file "project")
     (:module "registry"
      :serial t
      :components
      ((:file "git")))
     (:module "solver"
      :serial t
      :components
      ((:file "version")
       (:file "constraint")
       (:file "pubgrub")))
     (:file "fetch")
     (:module "build"
      :serial t
      :components
      ((:file "driver")
       (:file "orchestrator")))
     (:file "commands")
     (:file "main")))))
