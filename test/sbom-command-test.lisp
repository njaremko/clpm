;;;; test/sbom-command-test.lisp - `clpm deps sbom` command tests

(require :asdf)
(require :sb-posix)

;; Add repo root directory to ASDF load path.
(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

;; Load CLPM
(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "CLPM loaded successfully.~%~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing `clpm deps sbom`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (proj-root (merge-pathnames "proj/" tmp))
         (manifest (merge-pathnames "clpm.project" proj-root))
         (lock-path (merge-pathnames "clpm.lock" proj-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist proj-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           (uiop:with-current-directory (tmp)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("deps" "sbom"
                                     "--format" "cyclonedx-json"
                                     "--format" "spdx-json"))
               (declare (ignore stdout))
               (assert-eql 1 code)
               (assert-contains stderr "Duplicate option: --format")))

           ;; Create a minimal local git registry in CLPM_HOME/registries/main/.
           (let* ((reg-root (merge-pathnames "registries/main/" clpm-home))
                  (snapshot (merge-pathnames "registry/snapshot.sxp" reg-root))
                  (rel-a (merge-pathnames "registry/packages/a/1.0.0/release.sxp" reg-root))
                  (rel-b (merge-pathnames "registry/packages/b/2.0.0/release.sxp" reg-root)))
             (ensure-directories-exist snapshot)
             (ensure-directories-exist rel-a)
             (ensure-directories-exist rel-b)
             (clpm.io.sexp:write-canonical-sexp-to-file
              '(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
                :releases ("a@1.0.0" "b@2.0.0")
                :provides (("a" . "a@1.0.0")
                           ("b" . "b@2.0.0")))
              snapshot)
             (clpm.io.sexp:write-canonical-sexp-to-file
              '(:release :format 1
                :name "a" :version "1.0.0"
                :source (:tarball :url "https://example.invalid/a.tgz" :sha256 "aa")
                :artifact-sha256 "aa"
                :systems ("a")
                :system-deps ()
                :license "MIT")
              rel-a)
             (clpm.io.sexp:write-canonical-sexp-to-file
              '(:release :format 1
                :name "b" :version "2.0.0"
                :source (:tarball :url "https://example.invalid/b.tgz" :sha256 "bb" :sha1 "cc")
                :artifact-sha256 "bb"
                :systems ("b")
                :system-deps ()
                :license "Apache-2.0")
              rel-b))

           ;; Minimal project + lockfile that references those releases.
           (let* ((reg (clpm.registry:clone-registry "main" "file:///ignored" :kind :git :trust-key nil))
                  (meta-a (clpm.registry:get-release-metadata reg "a" "1.0.0"))
                  (meta-b (clpm.registry:get-release-metadata reg "b" "2.0.0")))
             (assert-true (and meta-a (string= "MIT" (clpm.registry:release-metadata-license meta-a)))
                          "Expected registry metadata for a to include license")
             (assert-true (and meta-b (string= "Apache-2.0" (clpm.registry:release-metadata-license meta-b)))
                          "Expected registry metadata for b to include license"))

           (clpm.project:write-project-file
            (clpm.project:make-project
             :name "proj"
             :version "0.1.0"
             :systems (list "proj")
             :depends '()
             :dev-depends '()
             :test-depends '()
             :registries '())
            manifest)

           (clpm.project:write-lock-file
            (clpm.project:make-lockfile
             :format 1
             :generated-at "2026-02-01T00:00:00Z"
             :project-name "proj"
             :clpm-version "0.1.0"
             :registries
             (list
              (clpm.project:make-locked-registry
               :name "main"
               :kind :git
               :url "file:///ignored"
               :trust nil))
             :resolved
             (list
              (clpm.project:make-locked-system
               :id "b"
               :release (clpm.project:make-locked-release
                         :name "b"
                         :version "2.0.0"
                         :source (clpm.project:make-locked-source
                                  :kind :tarball
                                  :url "https://example.invalid/b.tgz"
                                  :sha256 "bb"
                                  :sha1 "cc")
                         :artifact-sha256 "bb"
                         :tree-sha256 nil)
               :deps '())
              (clpm.project:make-locked-system
               :id "a"
               :release (clpm.project:make-locked-release
                         :name "a"
                         :version "1.0.0"
                         :source (clpm.project:make-locked-source
                                  :kind :tarball
                                  :url "https://example.invalid/a.tgz"
                                  :sha256 "aa"
                                  :sha1 nil)
                         :artifact-sha256 "aa"
                         :tree-sha256 nil)
               :deps '())))
            lock-path)

           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("deps" "sbom" "--format" "cyclonedx-json"))
               (assert-eql 0 code)
               (assert-true (string= "" stderr) "Expected empty stderr, got:~%~A" stderr)
               (assert-contains stdout "\"bomFormat\":\"CycloneDX\"")
               (assert-contains stdout "\"specVersion\":\"1.5\"")
               (assert-contains stdout "\"components\"")
               (assert-contains stdout "\"purl\":\"pkg:cl/a@1.0.0\"")
               (assert-contains stdout "\"purl\":\"pkg:cl/b@2.0.0\"")
               (assert-contains stdout "\"alg\":\"SHA-256\"")
               (assert-contains stdout "\"content\":\"aa\"")
               (assert-contains stdout "\"content\":\"bb\"")
               (assert-contains stdout "\"alg\":\"SHA-1\"")
               (assert-contains stdout "\"content\":\"cc\"")
               (assert-contains stdout "\"MIT\"")
               (assert-contains stdout "\"Apache-2.0\"")
               ;; Deterministic ordering: a component appears before b.
               (let ((pos-a (search "\"name\":\"a\"" stdout :test #'char-equal))
                     (pos-b (search "\"name\":\"b\"" stdout :test #'char-equal)))
                 (assert-true (and (integerp pos-a) (integerp pos-b) (< pos-a pos-b))
                              "Expected components to be sorted by name, got:~%~A"
                              stdout))))

           ;; Trusted registry metadata failures are not "license absent".
           (let ((original-lock (uiop:read-file-string lock-path)))
             (unwind-protect
                  (progn
                    (let* ((lock (clpm.project:read-lock-file lock-path))
                           (registry (first (clpm.project:lockfile-registries lock))))
                      (setf (clpm.project:locked-registry-trust registry)
                            "ed25519:missing")
                      (clpm.project:write-lock-file lock lock-path))
                    (uiop:with-current-directory (proj-root)
                      (multiple-value-bind (code stdout stderr)
                          (run-cli-captured '("deps" "sbom" "--format" "cyclonedx-json"))
                        (assert-true (not (zerop code))
                                     "Expected SBOM to fail on trusted registry load failure")
                        (assert-true (string= "" stdout)
                                     "Expected no SBOM stdout on trusted registry failure, got:~%~A"
                                     stdout)
                        (assert-contains stderr "Failed to load registry main"))))
               (with-open-file (s lock-path :direction :output
                                      :if-exists :supersede
                                      :external-format :utf-8)
                 (write-string original-lock s))))

           ;; Output file mode.
           (let ((out-json (merge-pathnames "sbom.json" proj-root)))
             (uiop:with-current-directory (proj-root)
               (multiple-value-bind (code _stdout stderr)
                   (run-cli-captured (list "deps" "sbom" "--format" "cyclonedx-json"
                                           "--output" (namestring out-json)))
                 (declare (ignore _stdout))
                 (assert-eql 1 code)
                 (assert-contains stderr "Unknown option: --output")))
             (uiop:with-current-directory (proj-root)
               (assert-eql 0 (clpm:run-cli (list "deps" "sbom" "--format" "cyclonedx-json"
                                                 "--out" (namestring out-json)))))
             (assert-true (uiop:file-exists-p out-json)
                          "Expected output file to exist: ~A" (namestring out-json))
             (let ((text (uiop:read-file-string out-json)))
               (assert-contains text "\"bomFormat\":\"CycloneDX\"")))

           ;; --- CycloneDX XML ----------------------------------------------
           (format t "Testing `--format cyclonedx-xml`...~%")
           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("deps" "sbom" "--format" "cyclonedx-xml"))
               (assert-eql 0 code)
               (assert-true (string= "" stderr) "Expected empty stderr, got:~%~A" stderr)
               (assert-contains stdout "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
               (assert-contains stdout "<bom xmlns=\"http://cyclonedx.org/schema/bom/1.5\"")
               (assert-contains stdout "<components>")
               (assert-contains stdout "<component type=\"library\">")
               (assert-contains stdout "<name>a</name>")
               (assert-contains stdout "<name>b</name>")
               (assert-contains stdout "<version>1.0.0</version>")
               (assert-contains stdout "<purl>pkg:cl/a@1.0.0</purl>")
               (assert-contains stdout "<purl>pkg:cl/b@2.0.0</purl>")
               (assert-contains stdout "<hash alg=\"SHA-256\">aa</hash>")
               (assert-contains stdout "<hash alg=\"SHA-256\">bb</hash>")
               (assert-contains stdout "<hash alg=\"SHA-1\">cc</hash>")
               (assert-contains stdout "<license><id>MIT</id></license>")
               (assert-contains stdout "<license><id>Apache-2.0</id></license>")
               (let ((pos-a (search "<name>a</name>" stdout :test #'char-equal))
                     (pos-b (search "<name>b</name>" stdout :test #'char-equal)))
                 (assert-true (and (integerp pos-a) (integerp pos-b) (< pos-a pos-b))
                              "Expected components to be sorted by name, got:~%~A"
                              stdout))))

           ;; --- SPDX JSON --------------------------------------------------
           (format t "Testing `--format spdx-json`...~%")
           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("deps" "sbom" "--format" "spdx-json"))
               (assert-eql 0 code)
               (assert-true (string= "" stderr) "Expected empty stderr, got:~%~A" stderr)
               (assert-contains stdout "\"spdxVersion\":\"SPDX-2.3\"")
               (assert-contains stdout "\"dataLicense\":\"CC0-1.0\"")
               (assert-contains stdout "\"SPDXID\":\"SPDXRef-DOCUMENT\"")
               (assert-contains stdout "\"name\":\"proj\"")
               (assert-contains stdout "\"documentNamespace\":")
               (assert-contains stdout "\"creationInfo\"")
               (assert-contains stdout "\"creators\":[\"Tool: clpm-0.1.0\"]")
               (assert-contains stdout "\"packages\":[")
               (assert-contains stdout "\"SPDXID\":\"SPDXRef-Package-a-1.0.0\"")
               (assert-contains stdout "\"SPDXID\":\"SPDXRef-Package-b-2.0.0\"")
               (assert-contains stdout "\"downloadLocation\":\"https://example.invalid/a.tgz\"")
               (assert-contains stdout "\"downloadLocation\":\"https://example.invalid/b.tgz\"")
               (assert-contains stdout "\"filesAnalyzed\":false")
               (assert-contains stdout "\"algorithm\":\"SHA256\"")
               (assert-contains stdout "\"checksumValue\":\"aa\"")
               (assert-contains stdout "\"checksumValue\":\"bb\"")
               (assert-contains stdout "\"algorithm\":\"SHA1\"")
               (assert-contains stdout "\"checksumValue\":\"cc\"")
               (assert-contains stdout "\"licenseConcluded\":\"MIT\"")
               (assert-contains stdout "\"licenseDeclared\":\"MIT\"")
               (assert-contains stdout "\"licenseConcluded\":\"Apache-2.0\"")
               (let ((pos-a (search "SPDXRef-Package-a-" stdout :test #'char-equal))
                     (pos-b (search "SPDXRef-Package-b-" stdout :test #'char-equal)))
                 (assert-true (and (integerp pos-a) (integerp pos-b) (< pos-a pos-b))
                              "Expected packages to be sorted by name, got:~%~A"
                              stdout))))

           ;; --- byte-stability across runs ---------------------------------
           (format t "Testing byte-for-byte stability across runs...~%")
           (dolist (fmt '("cyclonedx-json" "cyclonedx-xml" "spdx-json"))
             (uiop:with-current-directory (proj-root)
               (multiple-value-bind (c1 s1 e1)
                   (run-cli-captured (list "deps" "sbom" "--format" fmt))
                 (declare (ignore e1))
                 (multiple-value-bind (c2 s2 e2)
                     (run-cli-captured (list "deps" "sbom" "--format" fmt))
                   (declare (ignore e2))
                   (assert-eql 0 c1)
                   (assert-eql 0 c2)
                   (assert-true (string= s1 s2)
                                "~A output not byte-stable across runs.~%RUN1:~%~A~%RUN2:~%~A"
                                fmt s1 s2)))))

           ;; --- unsupported format rejected --------------------------------
           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code _ err)
                 (run-cli-captured '("deps" "sbom" "--format" "spdx-tag-value"))
               (declare (ignore _))
               (assert-true (not (zerop code))
                            "Expected non-zero rc for unsupported format")
               (assert-contains err "Unsupported --format"))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%SBOM command tests PASSED!~%")
(sb-ext:exit :code 0)
