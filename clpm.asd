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
       (:file "json")
       (:file "fs")))
     (:module "crypto"
      :serial t
      :components
      ((:file "sha1")
       (:file "sha256")
       (:file "sha512")
       (:file "ed25519")))
     (:file "platform")
     (:file "lisp")
     (:file "store")
     (:file "project")
     (:file "workspace")
     (:file "config")
     (:module "registry"
      :serial t
      :components
      ((:file "git")
       (:file "quicklisp")))
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
