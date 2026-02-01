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
  "Return user home directory."
  (or (uiop:getenv "HOME")
      (uiop:getenv "USERPROFILE")
      (user-homedir-pathname)))

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
     (merge-pathnames ".clpm/cache/" (home-dir)))))

(defun config-dir ()
  "Return CLPM config directory."
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
     (merge-pathnames ".clpm/config/" (home-dir)))))

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

;;; Process execution

(defun run-program (command &key (output :string) (error-output :output)
                              directory input)
  "Run an external program and return (values output error-output exit-code).
COMMAND is a list of strings (program and arguments).
OUTPUT can be :string, :lines, nil, or a stream.
ERROR-OUTPUT can be :string, :output (merge with output), nil, or a stream.
DIRECTORY is the working directory.
INPUT is a string or stream to pass to stdin."
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
                          :ignore-error-status t)
      (setf result-output (if (eq output :lines)
                              (uiop:split-string output-val :separator '(#\Newline))
                              output-val))
      (setf result-error (when (eq error-output :string) error-val))
      (values result-output result-error exit-code))))

(defun which (program)
  "Find program in PATH. Returns path or nil."
  (let ((cmd (if (eq *os-type* :windows) "where" "which")))
    (multiple-value-bind (output error-output exit-code)
        (run-program (list cmd program))
      (declare (ignore error-output))
      (when (zerop exit-code)
        (string-trim '(#\Space #\Newline #\Return) output)))))

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
