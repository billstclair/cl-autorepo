(in-package :cl-autorepo)

;;;
;;; cl-autorepo
;;; Automatically download repositories, and make them
;;; known to ASDF
;;;

(defparameter *autorepo-source-dir*
  (asdf:system-source-directory (asdf:find-system "cl-autorepo")))

(defparameter *repo-dir*
  (merge-pathnames "systems/" *autorepo-source-dir*))

(defparameter *autorepo-asdf-config-file*
  (merge-pathnames "autorepo.conf" *autorepo-source-dir*))

(defun add-system (name url &optional (repository-type :git))
  "Download the system named NAME of REPOSITORY-TYPE
from the URL, unless it's already defined."
  (initialize-autorepo-source-registry)
  (or (ignore-errors (asdf:find-system name))
      (progn (download-repo repository-type url *repo-dir*)
             (initialize-autorepo-source-registry)
             (asdf:find-system name))))

(defun initialize-autorepo-source-registry ()
  (asdf:initialize-source-registry *autorepo-asdf-config-file*))

(defgeneric download-repo (repository-type url directory)
  (:documentation "Download a repository of REPOSITORY-TYPE from URL, as a
subdirectory of DIRECTORY. REPOSITORY-TYPE can be :GIT, :SVN, :DARCS, or :HG"))

(defmethod download-repo ((repository-type (eql :git)) url directory)
  (asdf:run-shell-command "cd '~a'; git clone '~a'"
                          (truename directory) url))

(defmethod download-repo ((repository-type (eql :svn)) url directory)
  (asdf:run-shell-command "cd '~a'; svn co '~a'"
                          (truename directory) url))

(defmethod download-repo ((repository-type (eql :darcs)) url directory)
  (asdf:run-shell-command "cd '~a'; darcs get '~a'"
                          (truename directory) url))

(defmethod download-repo ((repository-type (eql :hg)) url directory)
  (asdf:run-shell-command "cd '~a'; hg clone '~a'"
                          (truename directory) url))
