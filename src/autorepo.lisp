(in-package :cl-autorepo)

;;;
;;; cl-autorepo
;;; Automatically download repositories, and make them
;;; known to ASDF
;;;

;; ASDF looks here by default
(defparameter *repo-dir*
  (merge-pathnames ".local/share/common-lisp/source/" (user-homedir-pathname)))

(defun add-system (name url &optional (repository-type :git))
  "Download the system named NAME of REPOSITORY-TYPE
from the URL, unless it's already defined."
  (initialize-autorepo-source-registry)
  (or (asdf:find-system name nil)
      (progn (download-repo repository-type url *repo-dir*)
             (initialize-autorepo-source-registry)
             (asdf:find-system name))))

(defun initialize-autorepo-source-registry ()
  (asdf:initialize-source-registry
   `(:source-registry
     :inherit-configuration
     (:tree ,*repo-dir*))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
