;;;; test/fetch-retry-test.lisp - retries and timeouts for HTTP fetch

(require :asdf)
(require :sb-posix)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

;;; ---- Test 1: transient failures recover within the retry budget ------------

(format t "Testing transient failures within retry budget...~%")

(let* ((tmp (clpm.platform:expand-path "/tmp/clpm-fetch-retry-test"))
       (dest (merge-pathnames "ok.bin" (uiop:ensure-directory-pathname tmp)))
       (attempts 0)
       (clpm.fetch::*test-fetcher*
         (lambda (url dest-path)
           (incf attempts)
           (cond
             ((< attempts 3)
              (error 'clpm.errors:clpm-fetch-error
                     :message "simulated transient failure"
                     :url url
                     :status 7))
             (t
              (ensure-directories-exist dest-path)
              (with-open-file (s dest-path :direction :output
                                           :element-type '(unsigned-byte 8)
                                           :if-exists :supersede)
                (write-sequence #(1 2 3 4) s))))))
       (clpm.fetch:*fetch-retries* 3)
       (clpm.fetch::*fetch-backoff-base* 0))  ; no real waits in tests
  (ensure-directories-exist (uiop:ensure-directory-pathname tmp))
  (clpm.fetch:fetch-url "https://example.invalid/x.bin" dest :progress nil)
  (assert-eql 3 attempts)
  (assert-true (uiop:file-exists-p dest) "Expected dest file to exist after retries"))
(format t "  recovered after transient failures~%")

;;; ---- Test 2: permanent failure exhausts the retry budget -------------------

(format t "Testing permanent failures exhaust the retry budget...~%")

(let* ((attempts 0)
       (clpm.fetch::*test-fetcher*
         (lambda (url dest-path)
           (declare (ignore dest-path))
           (incf attempts)
           (error 'clpm.errors:clpm-fetch-error
                  :message "permanent failure"
                  :url url
                  :status 22)))
       (clpm.fetch:*fetch-retries* 3)
       (clpm.fetch::*fetch-backoff-base* 0))
  (handler-case
      (progn
        (clpm.fetch:fetch-url "https://example.invalid/y.bin"
                              "/tmp/clpm-fetch-retry-test/y.bin"
                              :progress nil)
        (fail "expected fetch-url to raise after exhausting retries"))
    (clpm.errors:clpm-fetch-error (c)
      (declare (ignore c))
      nil))
  (assert-eql 3 attempts))
(format t "  exhausted budget and signaled error~%")

;;; ---- Test 3: env var CLPM_FETCH_RETRIES overrides default ------------------

(format t "Testing CLPM_FETCH_RETRIES env var override...~%")

(let ((old-env (sb-posix:getenv "CLPM_FETCH_RETRIES")))
  (unwind-protect
       (progn
         (sb-posix:setenv "CLPM_FETCH_RETRIES" "5" 1)
         (let* ((attempts 0)
                (clpm.fetch::*test-fetcher*
                  (lambda (url dest-path)
                    (declare (ignore dest-path))
                    (incf attempts)
                    (error 'clpm.errors:clpm-fetch-error
                           :message "boom"
                           :url url
                           :status 7)))
                (clpm.fetch:*fetch-retries* nil)
                (clpm.fetch::*fetch-backoff-base* 0))
           (handler-case
               (clpm.fetch:fetch-url "https://example.invalid/z.bin"
                                     "/tmp/clpm-fetch-retry-test/z.bin"
                                     :progress nil)
             (clpm.errors:clpm-fetch-error () nil))
           (assert-eql 5 attempts)))
    (if old-env
        (sb-posix:setenv "CLPM_FETCH_RETRIES" old-env 1)
        (sb-posix:unsetenv "CLPM_FETCH_RETRIES"))))
(format t "  env var override took effect~%")

;;; ---- Test 4: retries=1 means no retry --------------------------------------

(format t "Testing retries=1 (no retry)...~%")
(let* ((attempts 0)
       (clpm.fetch::*test-fetcher*
         (lambda (url dest-path)
           (declare (ignore dest-path))
           (incf attempts)
           (error 'clpm.errors:clpm-fetch-error
                  :message "once"
                  :url url
                  :status 7)))
       (clpm.fetch:*fetch-retries* 1)
       (clpm.fetch::*fetch-backoff-base* 0))
  (handler-case
      (clpm.fetch:fetch-url "https://example.invalid/once.bin"
                            "/tmp/clpm-fetch-retry-test/once.bin"
                            :progress nil)
    (clpm.errors:clpm-fetch-error () nil))
  (assert-eql 1 attempts))
(format t "  retries=1 attempted once~%")

;;; ---- Test 5: backoff schedule grows quadratically --------------------------

(format t "Testing backoff schedule...~%")
(let* ((delays '())
       (clpm.fetch::*test-fetcher*
         (lambda (url dest-path)
           (declare (ignore dest-path url))
           (error 'clpm.errors:clpm-fetch-error
                  :message "fail" :url "x" :status 7)))
       (clpm.fetch:*fetch-retries* 4)
       (clpm.fetch::*fetch-backoff-base* 1)
       (clpm.fetch::*fetch-sleep-fn*
         (lambda (s) (push s delays))))
  (handler-case
      (clpm.fetch:fetch-url "https://example.invalid/b.bin"
                            "/tmp/clpm-fetch-retry-test/b.bin"
                            :progress nil)
    (clpm.errors:clpm-fetch-error () nil))
  ;; 4 attempts -> 3 sleeps between them: 1, 4, 9 seconds.
  (setf delays (nreverse delays))
  (assert-true (equal delays '(1 4 9))
               "Expected delays (1 4 9), got ~S" delays))
(format t "  quadratic backoff confirmed~%")

(ignore-errors (uiop:delete-directory-tree
                (uiop:ensure-directory-pathname "/tmp/clpm-fetch-retry-test/")
                :validate t :if-does-not-exist :ignore))

(format t "~%Fetch retry tests PASSED!~%")
(sb-ext:exit :code 0)
