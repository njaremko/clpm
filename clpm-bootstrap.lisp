;;;; clpm-bootstrap.lisp - Bootstrap installer for CLPM
;;;;
;;;; This script installs CLPM without requiring Quicklisp or any external
;;;; dependencies beyond a working SBCL installation.
;;;;
;;;; Usage:
;;;;   sbcl --script clpm-bootstrap.lisp install
;;;;   sbcl --script clpm-bootstrap.lisp install --prefix /usr/local
;;;;   sbcl --script clpm-bootstrap.lisp uninstall
;;;;
;;;; Requirements:
;;;;   - SBCL 2.0.0 or later
;;;;   - curl or wget (for downloading)
;;;;   - tar (for extraction)

(defpackage #:clpm-bootstrap
  (:use #:cl))

(in-package #:clpm-bootstrap)

;;; Ensure ASDF/UIOP are available at read-time
;;;
;;; This file references `asdf:` and `uiop:` symbols inside function bodies.
;;; Those package prefixes are resolved by the reader, so we must ensure ASDF
;;; (and thus UIOP) is loaded *before* the reader encounters those forms.
;;;
;;; Some environments provide ASDF via an explicit file pointed to by $ASDF.
;;; If present, load it first, then fall back to `(require :asdf)`.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((asdf-path (sb-ext:posix-getenv "ASDF")))
    (when (and asdf-path (plusp (length asdf-path)))
      (load asdf-path)))
  (require :asdf))

;;; Configuration

(defparameter *clpm-version* "0.1.0")
(defparameter *clpm-repo* "https://github.com/clpm/clpm")
(defparameter *clpm-release-url*
  (format nil "~A/releases/download/v~A/clpm-~A-source.tar.gz"
          *clpm-repo* *clpm-version* *clpm-version*))

;;; Platform detection

(defun home-dir ()
  "Return the user's home directory as a directory pathname."
  (let ((home (or (sb-ext:posix-getenv "HOME")
                  (sb-ext:posix-getenv "USERPROFILE"))))
    (cond
      ((and home (plusp (length home)))
       (uiop:ensure-directory-pathname home))
      (t
       (uiop:ensure-directory-pathname (user-homedir-pathname))))))

(defun default-prefix ()
  (uiop:ensure-directory-pathname
   (merge-pathnames ".local/" (home-dir))))

(defun default-bin-dir ()
  (merge-pathnames "bin/" (default-prefix)))

(defun default-data-dir ()
  (merge-pathnames ".local/share/clpm/" (home-dir)))

;;; Utility functions

(defun run (command &key directory)
  "Run shell command and return (values output exit-code)."
  (let ((full-command (if directory
                          (format nil "cd ~A && ~A" (namestring directory) command)
                          command)))
    (with-output-to-string (out)
      (let ((process (sb-ext:run-program
                      "/bin/sh" (list "-c" full-command)
                      :output out
                      :error out
                      :wait t)))
        (values (get-output-stream-string out)
                (sb-ext:process-exit-code process))))))

(defun which (program)
  "Find program in PATH."
  (multiple-value-bind (output exit-code)
      (run (format nil "which ~A 2>/dev/null" program))
    (when (zerop exit-code)
      (string-trim '(#\Space #\Newline) output))))

(defun find-downloader ()
  "Find available downloader."
  (cond
    ((which "curl") :curl)
    ((which "wget") :wget)
    (t nil)))

(defun download (url dest)
  "Download URL to DEST."
  (let ((downloader (find-downloader)))
    (unless downloader
      (error "No downloader found. Please install curl or wget."))
    (format t "Downloading ~A...~%" url)
    (let ((command (ecase downloader
                     (:curl (format nil "curl -fsSL -o ~A ~A" dest url))
                     (:wget (format nil "wget -q -O ~A ~A" dest url)))))
      (multiple-value-bind (output exit-code)
          (run command)
        (unless (zerop exit-code)
          (error "Download failed: ~A" output))))))

(defun extract-tar-gz (archive dest)
  "Extract .tar.gz archive to dest."
  (format t "Extracting...~%")
  (ensure-directories-exist dest)
  (multiple-value-bind (output exit-code)
      (run (format nil "tar -xzf ~A -C ~A" archive dest))
    (unless (zerop exit-code)
      (error "Extraction failed: ~A" output))))

;;; Installation

(defun install (&key (prefix (default-prefix)))
  "Install CLPM."
  (format t "~%Installing CLPM ~A~%" *clpm-version*)
  (format t "Prefix: ~A~%~%" prefix)

  (let* ((tmp-dir (merge-pathnames
                   (format nil "clpm-install-~A/" (random (expt 2 32)))
                   "/tmp/"))
         (archive (merge-pathnames "clpm.tar.gz" tmp-dir))
         (extract-dir (merge-pathnames "extract/" tmp-dir))
         (bin-dir (merge-pathnames "bin/" prefix))
         (data-dir (default-data-dir)))

    (unwind-protect
         (progn
           ;; Create temp directory
           (ensure-directories-exist tmp-dir)

           ;; Download release
           (download *clpm-release-url* archive)

           ;; Extract
           (extract-tar-gz archive extract-dir)

           ;; Find extracted directory
           (let* ((contents (directory (merge-pathnames "*/" extract-dir)))
                  (source-dir (or (first contents) extract-dir)))

             ;; Build CLPM
             (format t "Building CLPM...~%")
             (let ((build-script (format nil "~
cd ~A
sbcl --noinform --non-interactive --disable-debugger \\
     --eval '(require :asdf)' \\
     --eval '(push ~S asdf:*central-registry*)' \\
     --eval '(asdf:load-system :clpm)' \\
     --eval '(clpm:build-executable ~S)'"
                                         (namestring source-dir)
                                         (namestring source-dir)
                                         (namestring (merge-pathnames "clpm" bin-dir)))))

               ;; Ensure bin directory exists
               (ensure-directories-exist bin-dir)

               (multiple-value-bind (output exit-code)
                   (run build-script)
                 (unless (zerop exit-code)
                   (error "Build failed:~%~A" output))))

             ;; Create data directory
             (ensure-directories-exist data-dir)

             ;; Success
             (format t "~%CLPM installed successfully!~%")
             (format t "~%Binary: ~A~%" (merge-pathnames "clpm" bin-dir))
             (format t "~%Make sure ~A is in your PATH.~%"
                     (namestring bin-dir))
             (format t "~%To verify installation:~%")
             (format t "  clpm --version~%~%")))

      ;; Cleanup
      (ignore-errors
       (run (format nil "rm -rf ~A" (namestring tmp-dir)))))))

(defun install-from-source (source-dir &key (prefix (default-prefix)))
  "Install CLPM from local source directory."
  (format t "~%Installing CLPM from ~A~%" source-dir)
  (format t "Prefix: ~A~%~%" prefix)

  (let ((bin-dir (merge-pathnames "bin/" prefix))
        (data-dir (default-data-dir)))

    ;; Build CLPM
    (format t "Building CLPM...~%")
    (ensure-directories-exist bin-dir)

    ;; Load and build
    (require :asdf)
    (push source-dir asdf:*central-registry*)
    (asdf:load-system :clpm)
    (funcall (find-symbol "BUILD-EXECUTABLE" :clpm)
             (merge-pathnames "clpm" bin-dir))

    ;; Create data directory
    (ensure-directories-exist data-dir)

    ;; Success
    (format t "~%CLPM installed successfully!~%")
    (format t "~%Binary: ~A~%" (merge-pathnames "clpm" bin-dir))))

(defun uninstall (&key (prefix (default-prefix)))
  "Uninstall CLPM."
  (format t "Uninstalling CLPM...~%")
  (let ((binary (merge-pathnames "bin/clpm" prefix)))
    (when (probe-file binary)
      (delete-file binary)
      (format t "Removed ~A~%" binary)))
  (format t "~%CLPM uninstalled.~%")
  (format t "Data directory at ~A was preserved.~%" (default-data-dir))
  (format t "Remove it manually if desired.~%"))

;;; Main

(defun print-usage ()
  (format t "CLPM Bootstrap Installer~%~%")
  (format t "Usage: sbcl --script clpm-bootstrap.lisp <command> [options]~%~%")
  (format t "Commands:~%")
  (format t "  install [--prefix DIR]     Install CLPM~%")
  (format t "  install-local DIR          Install from local source~%")
  (format t "  uninstall [--prefix DIR]   Uninstall CLPM~%")
  (format t "  help                       Show this help~%~%")
  (format t "Options:~%")
  (format t "  --prefix DIR    Installation prefix (default: ~~/.local)~%~%")
  (format t "Examples:~%")
  (format t "  sbcl --script clpm-bootstrap.lisp install~%")
  (format t "  sbcl --script clpm-bootstrap.lisp install --prefix /usr/local~%")
  (format t "  sbcl --script clpm-bootstrap.lisp install-local ./clpm~%")
  (format t "  sbcl --script clpm-bootstrap.lisp uninstall~%"))

(defun main (args)
  (let ((command (first args))
        (prefix (default-prefix))
        (source-dir nil))
    ;; Parse args (options may appear before/after positional args).
    (let ((rest (rest args))
          (positionals '()))
      (labels ((need (opt)
                 (let ((v (pop rest)))
                   (unless (and (stringp v) (plusp (length v)))
                     (format *error-output* "Missing value for ~A~%" opt)
                     (sb-ext:exit :code 1))
                   v))
               (looks-like-option-p (s)
                 (and (stringp s)
                      (plusp (length s))
                      (char= (char s 0) #\-))))
        (loop while rest do
          (let ((arg (pop rest)))
            (cond
              ((string= arg "--prefix")
               (setf prefix (uiop:ensure-directory-pathname (need "--prefix"))))
              ((string= arg "--source")
               (setf source-dir (uiop:ensure-directory-pathname (need "--source"))))
              ((looks-like-option-p arg)
               (format *error-output* "Unknown option: ~A~%" arg)
               (print-usage)
               (sb-ext:exit :code 1))
              (t
               (push arg positionals))))))
      (setf positionals (nreverse positionals))
      (when (and (null source-dir) positionals)
        (setf source-dir (uiop:ensure-directory-pathname (first positionals)))))

    ;; Dispatch
    (cond
      ((or (null command) (string= command "help") (string= command "--help"))
       (print-usage))
      ((string= command "install")
       (install :prefix prefix))
      ((string= command "install-local")
       (let ((dir (or source-dir (uiop:getcwd))))
         (install-from-source dir :prefix prefix)))
      ((string= command "uninstall")
       (uninstall :prefix prefix))
      (t
       (format *error-output* "Unknown command: ~A~%" command)
       (print-usage)
       (sb-ext:exit :code 1)))))

;;; Entry point when run as script

(main (uiop:command-line-arguments))
