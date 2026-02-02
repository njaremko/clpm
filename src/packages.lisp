;;;; packages.lisp - Package definitions for CLPM

(defpackage #:clpm.errors
  (:use #:cl)
  (:export #:clpm-error
           #:clpm-error-message
           #:clpm-user-error
           #:clpm-parse-error
           #:clpm-fetch-error
           #:clpm-hash-mismatch-error
           #:clpm-signature-error
           #:clpm-resolve-error
           #:clpm-resolve-error-systems
           #:clpm-resolve-error-conflict-chain
           #:clpm-resolve-error-explanation
           #:clpm-build-error
           #:clpm-missing-native-dep-error
           #:signal-error
           #:format-error))

(defpackage #:clpm.io.sexp
  (:use #:cl)
  (:export #:read-safe-sexp
           #:read-safe-sexp-from-string
           #:read-safe-sexp-from-file
           #:write-canonical-sexp
           #:write-canonical-sexp-to-string
           #:write-canonical-sexp-to-file
           #:read-manifest
           #:read-lockfile
           #:write-lockfile
           #:read-registry-snapshot
           #:read-release-metadata))

(defpackage #:clpm.io.fs
  (:use #:cl)
  (:export #:list-directory-entries
           #:walk-files))

(defpackage #:clpm.crypto.sha256
  (:use #:cl)
  (:export #:sha256
           #:sha256-stream
           #:sha256-file
           #:sha256-tree
           #:bytes-to-hex
           #:hex-to-bytes))

(defpackage #:clpm.crypto.sha1
  (:use #:cl)
  (:export #:sha1
           #:sha1-stream
           #:sha1-file
           #:sha1-files))

(defpackage #:clpm.crypto.sha512
  (:use #:cl)
  (:export #:sha512
           #:sha512-file))

(defpackage #:clpm.crypto.ed25519
  (:use #:cl)
  (:export #:verify-signature
           #:verify-file-signature
           #:load-public-key
           #:parse-key-id))

(defpackage #:clpm.platform
  (:use #:cl)
  (:export #:home-dir
           #:cache-dir
           #:config-dir
           #:data-dir
           #:store-dir
           #:registry-dir
           #:keys-dir
           #:log-dir
           #:platform-triple
           #:sbcl-version
           #:asdf-version
           #:features-hash
           #:ensure-directories
           #:run-program
           #:which
           #:expand-path
           #:find-downloader
           #:find-git
           #:find-tar))

(defpackage #:clpm.store
  (:use #:cl #:clpm.platform #:clpm.crypto.sha256)
  (:export #:store-artifact
           #:store-source
           #:store-build
           #:get-source-path
           #:get-artifact-path
           #:get-build-path
           #:artifact-exists-p
           #:source-exists-p
           #:build-exists-p
           #:compute-build-id
           #:gc-store
           #:with-temp-dir))

(defpackage #:clpm.project
  (:use #:cl #:clpm.io.sexp #:clpm.errors)
  (:export #:project
           #:make-project
           #:project-name
           #:project-version
           #:project-systems
           #:project-run
           #:project-test
           #:project-package
           #:project-depends
           #:project-dev-depends
           #:project-test-depends
           #:project-registries
           #:project-sbcl-constraints
           #:project-build-options
           #:project-scripts
           #:dependency
           #:make-dependency
           #:dependency-system
           #:dependency-constraint
           #:dependency-source
           #:dependency-optional-p
           #:dependency-features
           #:registry-ref
           #:registry-ref-name
           #:registry-ref-kind
           #:registry-ref-url
           #:registry-ref-trust
           #:lockfile
           #:make-lockfile
           #:lockfile-format
           #:lockfile-generated-at
           #:lockfile-project
           #:lockfile-registries
           #:lockfile-resolved
           #:locked-system
           #:locked-system-id
           #:locked-system-release
           #:locked-system-deps
           #:locked-release
           #:locked-release-name
           #:locked-release-version
           #:locked-release-source
           #:locked-release-artifact-sha256
           #:locked-release-tree-sha256
           #:locked-source
           #:make-locked-source
           #:locked-source-kind
           #:locked-source-url
           #:locked-source-sha256
           #:locked-source-sha1
           #:locked-source-commit
           #:locked-source-path
           #:make-locked-system
           #:make-locked-release
           #:make-locked-registry
           #:locked-registry
           #:locked-registry-name
           #:locked-registry-kind
           #:locked-registry-url
           #:locked-registry-commit
           #:locked-registry-trust
           #:locked-registry-signature
           #:read-project-file
           #:write-project-file
           #:read-lock-file
           #:write-lock-file
           #:find-project-root
           #:rfc3339-timestamp
           #:lockfile-project-name
           #:lockfile-clpm-version))

(defpackage #:clpm.registry
  (:use #:cl #:clpm.io.sexp #:clpm.crypto.sha256 #:clpm.crypto.ed25519 #:clpm.errors)
  (:export #:registry
           #:registry-name
           #:registry-kind
           #:registry-url
           #:registry-path
           #:registry-trust-key
           #:registry-snapshot-sig-sha256
           #:registry-snapshot
           #:registry-local-path
           #:snapshot
           #:snapshot-format
           #:snapshot-generated-at
           #:snapshot-releases
           #:snapshot-provides
           #:release-metadata
           #:release-metadata-name
           #:release-metadata-version
           #:release-metadata-source
           #:release-metadata-artifact-sha256
           #:release-metadata-systems
           #:release-metadata-system-deps
           #:release-metadata-native-requires
           #:release-metadata-license
           #:release-metadata-homepage
           #:release-metadata-description
           #:clone-registry
           #:update-registry
           #:load-registry
           #:find-system-candidates
           #:get-release-metadata
           #:build-registry-index
           #:registry-index
           #:index-lookup-system
           #:index-lookup-release
           #:git-rev-parse))

(defpackage #:clpm.config
  (:use #:cl #:clpm.io.sexp #:clpm.platform #:clpm.project #:clpm.errors)
  (:export #:config
           #:config-format
           #:config-registries
           #:config-defaults
           #:read-config
           #:write-config
           #:merge-project-config))

(defpackage #:clpm.solver.version
  (:use #:cl)
  (:export #:version
           #:version-p
           #:make-version
           #:version-major
           #:version-minor
           #:version-patch
           #:version-prerelease
           #:version-build
           #:version-raw
           #:parse-version
           #:version<
           #:version<=
           #:version>
           #:version>=
           #:version=
           #:version-to-string))

(defpackage #:clpm.solver.constraint
  (:use #:cl #:clpm.solver.version)
  (:export #:constraint
           #:range
           #:range-lo
           #:range-lo-inclusive-p
           #:range-hi
           #:range-hi-inclusive-p
           #:constraint-pinned-source
           #:parse-constraint
           #:constraint-satisfies-p
           #:constraint-intersect
           #:constraint-union
           #:constraint-empty-p
           #:any-constraint
           #:exact-constraint
           #:constraint-to-string))

(defpackage #:clpm.solver
  (:use #:cl #:clpm.solver.version #:clpm.solver.constraint #:clpm.errors)
  (:export #:solve
           #:resolution
           #:resolution-systems
           #:resolution-graph
           #:topological-sort
           #:resolution-to-lockfile
           #:conflict-explanation))

(defpackage #:clpm.fetch
  (:use #:cl #:clpm.platform #:clpm.store #:clpm.crypto.sha256 #:clpm.errors)
  (:export #:fetch-artifact
           #:fetch-git
           #:resolve-git-ref
           #:extract-archive
           #:verify-and-store
           #:fetch-lockfile-deps))

(defpackage #:clpm.build
  (:use #:cl #:clpm.platform #:clpm.store #:clpm.errors)
  (:export #:build-release
           #:build-all
           #:build-spec
           #:generate-asdf-config
           #:activate-project
           #:check-native-deps))

(defpackage #:clpm.commands
  (:use #:cl #:clpm.project #:clpm.registry #:clpm.solver #:clpm.fetch #:clpm.build
        #:clpm.store #:clpm.platform #:clpm.config #:clpm.errors)
  (:export #:*verbose*
           #:*offline*
           #:*insecure*
           #:*jobs*
           #:cmd-init
           #:cmd-new
           #:cmd-add
           #:cmd-remove
           #:cmd-resolve
           #:cmd-fetch
           #:cmd-build
           #:cmd-install
           #:cmd-update
           #:cmd-registry
           #:cmd-repl
           #:cmd-run
           #:cmd-exec
           #:cmd-test
           #:cmd-package
           #:cmd-clean
           #:cmd-gc
           #:cmd-help
           #:cmd-doctor))

(defpackage #:clpm
  (:use #:cl #:clpm.commands #:clpm.errors)
  (:export #:main
           #:run-cli
           #:*verbose*
           #:*offline*
           #:*insecure*
           #:*jobs*))
