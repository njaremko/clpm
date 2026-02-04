;;;; platform.lisp - Platform-specific utilities

(in-package #:clpm.platform)

;;; Platform detection at compile time

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *os-type*
    #+linux :linux
    #+darwin :darwin
    #+windows :windows
    #+freebsd :freebsd
    #+openbsd :openbsd
    #+netbsd :netbsd
    #-(or linux darwin windows freebsd openbsd netbsd) :unknown)

  (defparameter *arch-type*
    #+x86-64 :x86-64
    #+x86 :x86
    #+arm64 :arm64
    #+arm :arm
    #-(or x86-64 x86 arm64 arm) :unknown))

;;; Directory paths following XDG conventions

(defun home-dir ()
  "Return user home directory as a directory pathname."
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "HOME")
       (uiop:getenv "USERPROFILE")
       (user-homedir-pathname))))

(defun data-dir ()
  "Return CLPM data directory."
  (let ((override (uiop:getenv "CLPM_HOME")))
    (if override
        (uiop:ensure-directory-pathname override)
        (case *os-type*
          ((:linux :freebsd :openbsd :netbsd)
           (let ((xdg (uiop:getenv "XDG_DATA_HOME")))
             (if xdg
                 (merge-pathnames "clpm/" (uiop:ensure-directory-pathname xdg))
                 (merge-pathnames ".local/share/clpm/" (home-dir)))))
          (:darwin
           (merge-pathnames ".local/share/clpm/" (home-dir)))
          (:windows
           (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
             (if localappdata
                 (merge-pathnames "clpm/" (uiop:ensure-directory-pathname localappdata))
                 (merge-pathnames ".clpm/" (home-dir)))))
          (t
           (merge-pathnames ".clpm/" (home-dir)))))))

(defun cache-dir ()
  "Return CLPM cache directory."
  (let ((override (uiop:getenv "CLPM_HOME")))
    (if override
        (merge-pathnames "cache/" (uiop:ensure-directory-pathname override))
        (case *os-type*
          ((:linux :freebsd :openbsd :netbsd)
           (let ((xdg (uiop:getenv "XDG_CACHE_HOME")))
             (if xdg
                 (merge-pathnames "clpm/" (uiop:ensure-directory-pathname xdg))
                 (merge-pathnames ".cache/clpm/" (home-dir)))))
          (:darwin
           (merge-pathnames "Library/Caches/clpm/" (home-dir)))
          (:windows
           (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
             (if localappdata
                 (merge-pathnames "clpm/cache/" (uiop:ensure-directory-pathname localappdata))
                 (merge-pathnames ".clpm/cache/" (home-dir)))))
          (t
           (merge-pathnames ".clpm/cache/" (home-dir)))))))

(defun config-dir ()
  "Return CLPM config directory."
  (let ((override (uiop:getenv "CLPM_HOME")))
    (if override
        (merge-pathnames "config/" (uiop:ensure-directory-pathname override))
        (case *os-type*
          ((:linux :freebsd :openbsd :netbsd)
           (let ((xdg (uiop:getenv "XDG_CONFIG_HOME")))
             (if xdg
                 (merge-pathnames "clpm/" (uiop:ensure-directory-pathname xdg))
                 (merge-pathnames ".config/clpm/" (home-dir)))))
          (:darwin
           (merge-pathnames ".config/clpm/" (home-dir)))
          (:windows
           (let ((appdata (uiop:getenv "APPDATA")))
             (if appdata
                 (merge-pathnames "clpm/" (uiop:ensure-directory-pathname appdata))
                 (merge-pathnames ".clpm/config/" (home-dir)))))
          (t
           (merge-pathnames ".clpm/config/" (home-dir)))))))

(defun store-dir ()
  "Return CLPM store directory (content-addressed storage)."
  (merge-pathnames "store/" (cache-dir)))

(defun registry-dir ()
  "Return CLPM registry directory."
  (merge-pathnames "registries/" (data-dir)))

(defun keys-dir ()
  "Return CLPM keys directory."
  (merge-pathnames "keys/" (config-dir)))

(defun log-dir ()
  "Return CLPM log directory."
  (merge-pathnames "logs/" (cache-dir)))

;;; Platform information

(defun platform-triple ()
  "Return platform triple string: os-arch-libc."
  (let ((os (case *os-type*
              (:linux "linux")
              (:darwin "darwin")
              (:windows "windows")
              (:freebsd "freebsd")
              (:openbsd "openbsd")
              (:netbsd "netbsd")
              (t "unknown")))
        (arch (case *arch-type*
                (:x86-64 "x86_64")
                (:x86 "x86")
                (:arm64 "aarch64")
                (:arm "arm")
                (t "unknown")))
        (libc (case *os-type*
                (:linux "gnu")
                (:darwin "darwin")
                (:windows "msvc")
                (t "unknown"))))
    (format nil "~A-~A-~A" os arch libc)))

(defun sbcl-version ()
  "Return SBCL version string."
  (lisp-implementation-version))

(defun asdf-version ()
  "Return ASDF version string."
  (asdf:asdf-version))

(defun features-hash ()
  "Return hash of current *features* for build key."
  (let ((sorted-features (sort (mapcar #'symbol-name *features*) #'string<)))
    (clpm.crypto.sha256:bytes-to-hex
     (clpm.crypto.sha256:sha256
      (format nil "~{~A~^~%~}" sorted-features)))))

;;; Directory operations

(defun ensure-directories ()
  "Ensure all CLPM directories exist."
  (dolist (dir (list (data-dir) (cache-dir) (config-dir)
                     (store-dir) (registry-dir) (keys-dir) (log-dir)
                     (merge-pathnames "sources/" (store-dir))
                     (merge-pathnames "artifacts/" (store-dir))
                     (merge-pathnames "builds/" (store-dir))
                     (merge-pathnames "tmp/" (store-dir))))
    (ensure-directories-exist dir)))

;;; Secure randomness

(defun secure-random-bytes (n)
  "Return N cryptographically secure random bytes.

On Unix-like platforms this reads from /dev/urandom. Signals an error if no
secure randomness source is available."
  (unless (and (integerp n) (plusp n))
    (error "secure-random-bytes expects a positive integer, got ~S" n))
  (let ((urandom #P"/dev/urandom"))
    (unless (uiop:file-exists-p urandom)
      (error "No secure randomness source available (missing /dev/urandom)"))
    (with-open-file (stream urandom :element-type '(unsigned-byte 8))
      (let ((buf (make-array n :element-type '(unsigned-byte 8))))
        (let ((read (read-sequence buf stream)))
          (unless (= read n)
            (error "Failed to read ~D random bytes (got ~D)" n read)))
        buf))))

;;; Process execution

(defun run-program (command &key (output :string) (error-output :output)
                              directory input timeout
                              &allow-other-keys)
  "Run an external program and return (values output error-output exit-code).
COMMAND is a list of strings (program and arguments).
OUTPUT can be :string, :lines, nil, or a stream.
ERROR-OUTPUT can be :string, :output (merge with output), nil, or a stream.
DIRECTORY is the working directory.
INPUT is a string or stream to pass to stdin.
TIMEOUT, when non-nil, is a number of seconds to allow the process to run."
  ;; UIOP's `run-program` may try to load implementation contribs (e.g. sb-posix)
  ;; on some SBCL distributions. Prefer SBCL's native runner when available.
  (labels ((slurp-stream (stream)
             (let ((buf (make-string 4096)))
               (with-output-to-string (out)
                 (loop
                   (let ((n (read-sequence buf stream)))
                     (when (zerop n)
                       (return))
                     (write-string buf out :end n))))))
           (normalize-io (x)
             (case x
               (:string :stream)
               (:lines :stream)
               (:interactive t)
               (t x)))
           (normalize-input (x)
             (cond
               ((null x) nil)
               ((stringp x) (make-string-input-stream x))
               (t x)))
           (maybe-split-lines (s)
             (uiop:split-string s :separator '(#\Newline)))
           (wait-with-timeout (proc timeout-seconds)
             (when (null timeout-seconds)
               (sb-ext:process-wait proc)
               (return-from wait-with-timeout t))
             (unless (and (numberp timeout-seconds) (plusp timeout-seconds))
               (sb-ext:process-wait proc)
               (return-from wait-with-timeout t))
             (let* ((deadline (+ (get-internal-real-time)
                                 (* timeout-seconds internal-time-units-per-second))))
               (loop while (sb-ext:process-alive-p proc) do
                 (when (>= (get-internal-real-time) deadline)
                   (ignore-errors (sb-ext:process-kill proc 15))
                   (sleep 0.05)
                   (ignore-errors (sb-ext:process-kill proc 9))
                   (return-from wait-with-timeout nil))
                 (sleep 0.01))
               t)))
    #+sbcl
    (let* ((command (or command '()))
           (program (first command))
           (args (rest command)))
      (unless (and (stringp program) (plusp (length program)))
        (error "run-program expects a non-empty command list, got ~S" command))
      (let* ((out-spec (normalize-io output))
             (err-spec (if (eq error-output :output)
                           :output
                           (normalize-io error-output)))
             (in-spec (normalize-input input))
             (proc (sb-ext:run-program program args
                                       :search t
                                       :wait nil
                                       :directory directory
                                       :input (or in-spec nil)
                                       :output out-spec
                                       :error err-spec)))
        (unwind-protect
             (let ((ok (wait-with-timeout proc timeout)))
               (cond
                 ((not ok)
                  (values (if (eq output :lines) '() "")
                          (when (eq error-output :string) "Process timed out")
                          124))
                 (t
                  (let* ((exit-code (or (sb-ext:process-exit-code proc) 1))
                         (out-str (when (eq out-spec :stream)
                                    (let ((s (sb-ext:process-output proc)))
                                      (when s (slurp-stream s)))))
                         (err-str (when (eq err-spec :stream)
                                    (let ((s (sb-ext:process-error proc)))
                                      (when s (slurp-stream s))))))
                    (values (cond
                              ((eq output :lines) (maybe-split-lines (or out-str "")))
                              ((eq output :string) (or out-str ""))
                              (t out-str))
                            (when (eq error-output :string)
                              (or err-str ""))
                            exit-code)))))
          (ignore-errors
           (let ((s (sb-ext:process-output proc)))
             (when (streamp s) (close s))))
          (ignore-errors
           (let ((s (sb-ext:process-error proc)))
             (when (and (streamp s) (not (eq err-spec :output))) (close s))))
          (ignore-errors
           (let ((s (sb-ext:process-input proc)))
             (when (streamp s) (close s)))))))
    #-sbcl
    (let ((result-output nil)
          (result-error nil))
      (multiple-value-bind (output-val error-val exit-code)
          (uiop:run-program command
                            :output (case output
                                      (:string :string)
                                      (:lines :string)
                                      (t output))
                            :error-output (case error-output
                                            (:string :string)
                                            (:output :output)
                                            (t error-output))
                            :directory directory
                            :input input
                            :timeout timeout
                            :ignore-error-status t)
        (setf result-output (if (eq output :lines)
                                (uiop:split-string output-val :separator '(#\Newline))
                                output-val))
        (setf result-error (when (eq error-output :string) error-val))
        (values result-output result-error exit-code)))))

(defun which (program)
  "Find PROGRAM on PATH without relying on external `which`.

Returns an absolute path string or NIL."
  (labels ((nonempty (s) (and (stringp s) (plusp (length s))))
           (path-separator ()
             (if (eq *os-type* :windows) #\; #\:))
           (split-path (s)
             (let ((sep (path-separator)))
               (remove-if (lambda (p) (or (null p) (string= p "")))
                          (uiop:split-string s :separator (list sep)))))
           (pathlike-p (s)
             (or (find #\/ s)
                 (find #\\ s)))
           (win-exts ()
             (let* ((raw (or (uiop:getenv "PATHEXT")
                             ".COM;.EXE;.BAT;.CMD"))
                    (exts (uiop:split-string raw :separator '(#\;))))
               (remove-if (lambda (e) (or (null e) (string= e ""))) exts)))
           (ensure-exe (p)
             (let ((found (uiop:file-exists-p p)))
               (when found
                 (namestring found)))))
    (unless (nonempty program)
      (return-from which nil))
    ;; If PROGRAM contains path separators, treat it as a path and do not
    ;; consult PATH.
    (when (pathlike-p program)
      (return-from which
        (ensure-exe (uiop:ensure-pathname program
                                          :defaults (uiop:getcwd)
                                          :want-existing nil))))
    (let* ((path (or (uiop:getenv "PATH") ""))
           (dirs (split-path path)))
      (dolist (dir dirs)
        (let* ((dir (uiop:ensure-directory-pathname dir))
               (base (merge-pathnames program dir))
               (found (ensure-exe base)))
          (when found
            (return-from which found))
          (when (eq *os-type* :windows)
            (dolist (ext (win-exts))
              (let ((cand (merge-pathnames (concatenate 'string program ext) dir)))
                (let ((found (ensure-exe cand)))
                  (when found
                    (return-from which found))))))))
      nil)))

;;; Environment helpers

(defun getenv-path (var)
  "Get environment variable as pathname."
  (let ((val (uiop:getenv var)))
    (when val
      (uiop:ensure-directory-pathname val))))

(defun expand-path (path)
  "Expand ~ in path."
  (if (and (stringp path)
           (plusp (length path))
           (char= (char path 0) #\~))
      (merge-pathnames (subseq path 2) (home-dir))
      path))

;;; Downloader detection

(defun find-downloader ()
  "Find an available downloader. Returns :curl, :wget, or :powershell."
  (cond
    ((which "curl") :curl)
    ((which "wget") :wget)
    ((and (eq *os-type* :windows) (which "powershell.exe")) :powershell)
    (t nil)))

(defun find-git ()
  "Find git executable. Returns path or nil."
  (which "git"))

(defun find-tar ()
  "Find tar executable. Returns path or nil."
  (or (which "tar")
      (and (eq *os-type* :windows) (which "tar.exe"))))

(defun tool-install-hints (tool)
  "Return a list of human-readable install hints for TOOL (a program name)."
  (let* ((tool (string-downcase (or tool "")))
         (is-mac (uiop:os-macosx-p))
         (is-win (uiop:os-windows-p)))
    (labels ((mac (pkg)
               (format nil "macOS (Homebrew): brew install ~A" pkg))
             (deb (pkg)
               (format nil "Debian/Ubuntu: sudo apt-get install ~A" pkg))
             (fed (pkg)
               (format nil "Fedora: sudo dnf install ~A" pkg))
             (win (pkg)
               (format nil "Windows (chocolatey): choco install ~A" pkg)))
      (cond
        ((string= tool "git")
         (remove nil (list (and is-mac (mac "git"))
                           (and (not is-win) (deb "git"))
                           (and (not is-win) (fed "git"))
                           (and is-win (win "git")))))
        ((or (string= tool "tar") (string= tool "tar.exe"))
         (remove nil (list (and is-mac "macOS: tar is typically preinstalled (or install Xcode Command Line Tools)")
                           (and (not is-win) (deb "tar"))
                           (and (not is-win) (fed "tar"))
                           (and is-win "Windows: tar is included in recent Windows 10/11; otherwise install bsdtar or add tar.exe to PATH"))))
        ((string= tool "sbcl")
         (remove nil (list (and is-mac (mac "sbcl"))
                           (and (not is-win) (deb "sbcl"))
                           (and (not is-win) (fed "sbcl"))
                           (and is-win "Windows: install SBCL and ensure sbcl.exe is on PATH"))))
        ((string= tool "ecl")
         (remove nil (list (and is-mac (mac "ecl"))
                           (and (not is-win) (deb "ecl"))
                           (and (not is-win) (fed "ecl"))
                           (and is-win "Windows: install ECL and ensure ecl.exe is on PATH"))))
        ((string= tool "ccl")
         (remove nil (list (and is-mac (mac "clozure-cl"))
                           (and (not is-win) "Linux: install Clozure CL and ensure `ccl` is on PATH")
                           (and is-win "Windows: install Clozure CL and ensure ccl.exe is on PATH"))))
        (t
         (remove nil (list (and is-mac (format nil "macOS: ensure `~A` is on PATH (Homebrew recommended)" tool))
                           (and (not is-win) (format nil "Linux: install `~A` and ensure it is on PATH" tool))
                           (and is-win (format nil "Windows: install `~A` and ensure it is on PATH" tool)))))))))
