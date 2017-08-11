;;; easy-jekyll.el --- Major mode managing jekyll blogs -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-easy-jekyll
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs major mode for writing blogs made with jekyll
;; You can publish your blog to the server or Github Pages
;; or Amazon S3 or Google Cloud Storage.

;;; Code:

(require 'cl-lib)

(defgroup easy-jekyll nil
  "Major mode managing jekyll blogs."
  :group 'tools)

(defgroup easy-jekyll-faces nil
  "Faces used in `easy-jekyll'"
  :group 'easy-jekyll :group 'faces)

(defcustom easy-jekyll-basedir nil
  "Directory where jekyll html source code is placed."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-preview-url "http://localhost:4000/"
  "Preview url of easy-jekyll."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url nil
  "Url of the site operated by jekyll."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain nil
  "Domain of jekyll at your ~/.ssh/config."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root nil
  "Root directory of jekyll at your server."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-previewtime 300
  "Preview display time."
  :group 'easy-jekyll
  :type 'integer)

(defcustom easy-jekyll-amazon-s3-bucket-name nil
  "Amazon S3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name nil
  "Google Cloud Storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-default-ext ".md"
  "Default extension when posting new articles."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-no-help nil
  "No help flg of easy-jekyll."
  :group 'easy-jekyll
  :type 'integer)

(defcustom easy-jekyll-sort-default-char nil
  "Default setting to sort with charactor."
  :group 'easy-jekyll
  :type 'integer)

(defcustom easy-jekyll-publish-chmod "Du=rwx,Dgo=rx,Fu=rw,Fog=r"
  "Permission when publish.The default is drwxr-xr-x."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-github-deploy-script "deploy.sh"
  "Github-deploy-script file name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-markdown-extension "md"
  "Markdown extension."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-textile-extension "textile"
  "Textile extension."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-postdir "_post"
  "Directory where the theme store it's posts."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-blog-number nil
  "Number of your blog which you managed."
  :group 'easy-jekyll
  :type 'integer)

(defcustom easy-jekyll-basedir-1 nil
  "Blog1 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-1 nil
  "Blog1 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-1 nil
  "Blog1 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-1 nil
  "Blog1 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-1 nil
  "Blog1 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-1 nil
  "Blog1 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-2 nil
  "Blog2 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-2 nil
  "Blog2 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-2 nil
  "Blog2 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-2 nil
  "Blog2 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-2 nil
  "Blog2 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-2 nil
  "Blog2 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-3 nil
  "Blog3 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-3 nil
  "Blog3 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-3 nil
  "Blog3 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-3 nil
  "Blog3 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-3 nil
  "Blog3 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-3 nil
  "Blog3 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-4 nil
  "Blog1 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-4 nil
  "Blog4 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-4 nil
  "Blog4 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-4 nil
  "Blog4 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-4 nil
  "Blog4 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom  easy-jekyll-google-cloud-storage-bucket-name-4 nil
  "Blog4 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-5 nil
  "Blog5 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-5 nil
  "Blog5 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-5 nil
  "Blog5 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-5 nil
  "Blog5 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-5 nil
  "Blog5 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-5 nil
  "Blog5 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-6 nil
  "Blog6 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-6 nil
  "Blog6 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-6 nil
  "Blog6 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-6 nil
  "Blog6 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-6 nil
  "Blog6 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-6 nil
  "Blog6 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-7 nil
  "Blog7 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-7 nil
  "Blog7 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-7 nil
  "Blog7 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-7 nil
  "Blog7 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-7 nil
  "Blog7 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-7 nil
  "Blog7 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-8 nil
  "Blog8 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-8 nil
  "Blog8 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-8 nil
  "Blog8 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-8 nil
  "Blog8 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom  easy-jekyll-amazon-s3-bucket-name-8 nil
  "Blog8 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-8 nil
  "Blog8 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-basedir-9 nil
  "Blog9 base directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-url-9 nil
  "Blog9 url."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-root-9 nil
  "Blog1 root."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-sshdomain-9 nil
  "Blog9 sshdomain."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-amazon-s3-bucket-name-9 nil
  "Blog9 amazon s3 bucket name."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-google-cloud-storage-bucket-name-9 nil
  "Blog9 google cloud storage bucket name."
  :group 'easy-jekyll
  :type 'string)

(defvar easy-jekyll--server-process nil
  "Jekyll process.")

(defvar easy-jekyll--unmovable-line 10
  "Impossible to move below this line.")

(defvar easy-jekyll--draft-list nil
  "Draft list flg.")

(defvar easy-jekyll--draft-mode nil
  "Display draft-mode.")

(defvar easy-jekyll--publish-timer nil
  "Easy-jekyll-publish-timer.")

(defvar easy-jekyll--basedir-timer nil
  "Easy-jekyll-basedir-timer.")

(defvar easy-jekyll--sshdomain-timer nil
  "Easy-jekyll-sshdomain-timer.")

(defvar easy-jekyll--root-timer nil
  "Easy-jekyll-root-timer.")

(defvar easy-jekyll--url-timer nil
  "Easy-jekyll-url-timer.")

(defvar easy-jekyll--github-deploy-timer nil
  "Easy-jekyll-github-deploy-timer.")

(defvar easy-jekyll--github-deploy-basedir-timer nil
  "Easy-jekyll-github-deploy-basedir-timer.")

(defvar easy-jekyll--github-deploy-url-timer nil
  "Easy-jekyll-github-deploy-url-timer.")

(defvar easy-jekyll--amazon-s3-timer nil
  "Easy-jekyll-amazon-s3-timer.")

(defvar easy-jekyll--amazon-s3-basedir-timer nil
  "Easy-jekyll-amazon-s3-basedir-timer.")

(defvar easy-jekyll--amazon-s3-url-timer nil
  "Easy-jekyll-amazon-s3-url-timer.")

(defvar easy-jekyll--amazon-s3-bucket-name-timer nil
  "Easy-jekyll-amazon-s3-bucket-name-timer.")

(defvar easy-jekyll--google-cloud-storage-timer nil
  "Easy-jekyll-google-cloud-storage-timer.")

(defvar easy-jekyll--google-cloud-storage-basedir-timer nil
  "Easy-jekyll-google-cloud-storage-basedir-timer.")

(defvar easy-jekyll--google-cloud-storage-url-timer nil
  "Easy-jekyll-google-cloud-storage-url-timer.")

(defvar easy-jekyll--google-cloud-storage-bucket-name-timer nil
  "Easy-jekyll-google-cloud-storage-bucket-name-timer.")

(defvar easy-jekyll--publish-basedir nil
  "Easy-jekyll-publish-var.")

(defvar easy-jekyll--publish-sshdomain nil
  "Easy-jekyll-publish-var.")

(defvar easy-jekyll--publish-root nil
  "Easy-jekyll-publish-var.")

(defvar easy-jekyll--publish-url nil
  "Easy-jekyll-publish-var.")

(defvar easy-jekyll--github-deploy-basedir nil
  "Easy-jekyll-github-deploy-var.")

(defvar easy-jekyll--github-deploy-url nil
  "Easy-jekyll-github-deploy-var.")

(defvar easy-jekyll--amazon-s3-basedir nil
  "Easy-jekyll-amazon-s3-var.")

(defvar easy-jekyll--amazon-s3-url nil
  "Easy-jekyll-amazon-s3-var.")

(defvar easy-jekyll--amazon-s3-bucket-name nil
  "Easy-jekyll-amazon-s3-var.")

(defvar easy-jekyll--google-cloud-storage-basedir nil
  "Easy-jekyll-google-cloud-storage-var.")

(defvar easy-jekyll--google-cloud-storage-url nil
  "Easy-jekyll-google-cloud-storage-var.")

(defvar easy-jekyll--google-cloud-storage-bucket-name nil
  "Easy-jekyll-google-cloud-storage-var.")

(defconst easy-jekyll--unmovable-line-default easy-jekyll--unmovable-line
  "Default value of impossible to move below this line.")

(defconst easy-jekyll--delete-line 11
  "Easy-jekyll-delete line number.")

(defconst easy-jekyll--buffer-name "*Jekyll Serve*"
  "Easy-jekyll buffer name.")

(defconst easy-jekyll--preview-buffer "*Jekyll Preview*"
  "Easy-jekyll preview buffer name.")

(defconst easy-jekyll--formats `(,easy-jekyll-markdown-extension
				 ,easy-jekyll-textile-extension))

(defface easy-jekyll-help-face
  '((((class color) (background light)) (:bold t :foreground "#82c600" :background "#f0f8ff"))
    (((class color) (background dark)) (:bold t :foreground "#82c600" :background "#2f4f4f")))
  ""
  :group 'easy-jekyll-faces)

;;;###autoload
(defun easy-jekyll-article ()
  "Open a list of articles written in jekyll with dired."
  (interactive)
  (unless easy-jekyll-basedir
    (error "Please set easy-jekyll-basedir variable"))
  (find-file (expand-file-name easy-jekyll-postdir easy-jekyll-basedir)))

(defmacro easy-jekyll-with-env (&rest body)
  "Evaluate BODY with `default-directory' set to `easy-jekyll-basedir'.
Report an error if jekyll is not installed, or if `easy-jekyll-basedir' is unset."
  `(progn
     (unless easy-jekyll-basedir
       (error "Please set easy-jekyll-basedir variable"))
     (unless (executable-find "jekyll")
       (error "'jekyll' is not installed"))
     (let ((default-directory easy-jekyll-basedir))
       ,@body)))

;;;###autoload
(defun easy-jekyll-publish ()
  "Adapt local change to the server with jekyll."
  (interactive)
  (unless easy-jekyll-sshdomain
    (error "Please set easy-jekyll-sshdomain variable"))
  (unless easy-jekyll-root
    (error "Please set easy-jekyll-root variable"))
  (unless (executable-find "rsync")
    (error "'rsync' is not installed"))
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (easy-jekyll-with-env
   (when (file-directory-p "_site")
     (delete-directory "_site" t nil))
   (shell-command-to-string "jekyll build --destination _site")
   (shell-command-to-string (concat "rsync -rtpl --chmod=" easy-jekyll-publish-chmod " --delete _site/ " easy-jekyll-sshdomain ":" (shell-quote-argument easy-jekyll-root)))
   (message "Blog published")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-publish-timer(n)
  "A timer that publish after the specified number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--sshdomain-timer easy-jekyll-sshdomain)
  (setq easy-jekyll--root-timer easy-jekyll-root)
  (setq easy-jekyll--url-timer easy-jekyll-url)
  (setq easy-jekyll--publish-timer
	(run-at-time (* n 60) nil #'easy-jekyll-publish-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-publish-timer()
  "Cancel timer that publish after the specified number of minutes has elapsed."
  (interactive)
  (when easy-jekyll--publish-timer
    (cancel-timer easy-jekyll--publish-timer)
    (setq easy-jekyll--publish-timer nil)
    (message "Easy-jekyll-publish-timer canceled")))

(defun easy-jekyll-publish-on-timer ()
  "Adapt local change to the server with jekyll on timer."
  (setq easy-jekyll--publish-basedir easy-jekyll-basedir)
  (setq easy-jekyll-basedir easy-jekyll--basedir-timer)
  (setq easy-jekyll--publish-sshdomain easy-jekyll-sshdomain)
  (setq easy-jekyll-sshdomain easy-jekyll--sshdomain-timer)
  (setq easy-jekyll--publish-root easy-jekyll-root)
  (setq easy-jekyll-root easy-jekyll--root-timer)
  (setq easy-jekyll--publish-url easy-jekyll-url)
  (setq easy-jekyll-url easy-jekyll--url-timer)
  (easy-jekyll-publish)
  (setq easy-jekyll-basedir easy-jekyll--publish-basedir)
  (setq easy-jekyll-sshdomain easy-jekyll--publish-sshdomain)
  (setq easy-jekyll-root easy-jekyll--publish-root)
  (setq easy-jekyll-url easy-jekyll--publish-url))

(defun easy-jekyll--headers (file)
  "Return a draft header string for a new article as FILE."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%d %T ")
          (format-time-string "%z"))))
    (concat
     "---"
     "\nlayout: post"
     "\ntitle:  " file
     "\ndate:   " datetimezone
     "\ncategories: "
     "\n---\n")))

;;;###autoload
(defun easy-jekyll-newpost (post-file)
  "Create a new post with jekyll.
POST-FILE needs to have and extension '.md' or '.textile'."
  (interactive (list (read-from-minibuffer "Filename: " `(,easy-jekyll-default-ext . 1) nil nil nil)))
  (let ((filename (concat easy-jekyll-postdir "/" (format-time-string "%Y-%m-%d-" (current-time)) post-file))
	(file-ext (file-name-extension post-file)))
    (when (not (member file-ext easy-jekyll--formats))
      (error "Please enter .%s file name or .%s file name" easy-jekyll-markdown-extension easy-jekyll-textile-extension))
    (easy-jekyll-with-env
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" (concat easy-jekyll-basedir filename)))
     (find-file (concat "content/" filename))
     (insert (easy-jekyll--headers (file-name-base post-file)))
     (goto-char (point-max))
     (save-buffer))))

;;;###autoload
(defun easy-jekyll-preview ()
  "Preview jekyll at localhost."
  (interactive)
  (easy-jekyll-with-env
   (if (process-live-p easy-jekyll--server-process)
       (browse-url easy-jekyll-preview-url)
     (progn
       (if (<= 0.25 (easy-jekyll--version))
	   (setq easy-jekyll--server-process
	   	 (start-process "jekyll-server" easy-jekyll--preview-buffer "jekyll" "server" "--navigateToChanged"))
	 (setq easy-jekyll--server-process
	       (start-process "jekyll-server" easy-jekyll--preview-buffer "jekyll" "server")))
       (browse-url easy-jekyll-preview-url)
       (run-at-time easy-jekyll-previewtime nil 'easy-jekyll--preview-end)))))

(defun easy-jekyll--preview-end ()
  "Finish previewing jekyll at localhost."
  (unless (null easy-jekyll--server-process)
    (delete-process easy-jekyll--server-process))
  (when (get-buffer easy-jekyll--preview-buffer)
    (kill-buffer easy-jekyll--preview-buffer)))

;;;###autoload
(defun easy-jekyll-github-deploy ()
  "Execute `easy-jekyll-github-deploy-script' script locate at `easy-jekyll-basedir'."
  (interactive)
  (easy-jekyll-with-env
   (let ((deployscript (file-truename (concat easy-jekyll-basedir easy-jekyll-github-deploy-script))))
     (unless (executable-find deployscript)
       (error "%s do not execute" deployscript))
     (shell-command-to-string (shell-quote-argument deployscript))
     (message "Blog deployed")
     (when easy-jekyll-url
       (browse-url easy-jekyll-url)))))

;;;###autoload
(defun easy-jekyll-github-deploy-timer(n)
  "A timer that github-deploy after the specified number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--github-deploy-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--github-deploy-url-timer easy-jekyll-url)
  (setq easy-jekyll--github-deploy-timer
	(run-at-time (* n 60) nil #'easy-jekyll-github-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-github-deploy-timer()
  "Cancel timer that github-deploy after the specified number of minutes has elapsed."
  (interactive)
  (when easy-jekyll--github-deploy-timer
    (cancel-timer easy-jekyll--github-deploy-timer)
    (setq easy-jekyll--github-deploy-timer nil)
    (message "Easy-jekyll-github-deploy-timer canceled")))

(defun easy-jekyll-github-deploy-on-timer ()
  "Execute `easy-jekyll-github-deploy-script' script on timer locate at `easy-jekyll-basedir'."
  (setq easy-jekyll--github-deploy-basedir easy-jekyll-basedir)
  (setq easy-jekyll-basedir easy-jekyll--github-deploy-basedir-timer)
  (setq easy-jekyll--github-deploy-url easy-jekyll-url)
  (setq easy-jekyll-url easy-jekyll--github-deploy-url-timer)
  (easy-jekyll-github-deploy)
  (setq easy-jekyll-basedir easy-jekyll--github-deploy-basedir)
  (setq easy-jekyll-url easy-jekyll--github-deploy-url))

;;;###autoload
(defun easy-jekyll-amazon-s3-deploy ()
  "Deploy jekyll source at Amazon S3."
  (interactive)
  (easy-jekyll-with-env
   (unless (executable-find "aws")
     (error "'aws' is not installed"))
   (unless easy-jekyll-amazon-s3-bucket-name
     (error "Please set 'easy-jekyll-amazon-s3-bucket-name' variable"))
   (when (file-directory-p "public")
     (delete-directory "public" t nil))
   (shell-command-to-string "jekyll --destination public")
   (shell-command-to-string (concat "aws s3 sync --delete public s3://" easy-jekyll-amazon-s3-bucket-name "/"))
   (message "Blog deployed")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-amazon-s3-deploy-timer(n)
  "A timer that amazon-s3-deploy after the specified number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--amazon-s3-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--amazon-s3-url-timer easy-jekyll-url)
  (setq easy-jekyll--amazon-s3-bucket-name-timer easy-jekyll-amazon-s3-bucket-name)
  (setq easy-jekyll--amazon-s3-timer
	(run-at-time (* n 60) nil #'easy-jekyll-amazon-s3-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-amazon-s3-deploy-timer()
  "Cancel timer that amazon-s3-deploy after the specified number of minutes has elapsed."
  (interactive)
  (when easy-jekyll--amazon-s3-timer
    (cancel-timer easy-jekyll--amazon-s3-timer)
    (setq easy-jekyll--amazon-s3-timer nil)
    (message "Easy-jekyll-amazon-s3-deploy-timer canceled")))

(defun easy-jekyll-amazon-s3-deploy-on-timer ()
  "Deploy jekyll source at Amazon S3 on timer."
  (setq easy-jekyll--amazon-s3-basedir easy-jekyll-basedir)
  (setq easy-jekyll-basedir easy-jekyll--amazon-s3-basedir-timer)
  (setq easy-jekyll--amazon-s3-url easy-jekyll-url)
  (setq easy-jekyll-url easy-jekyll--amazon-s3-url-timer)
  (setq easy-jekyll--amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name)
  (setq easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-timer)
  (easy-jekyll-amazon-s3-deploy)
  (setq easy-jekyll-basedir easy-jekyll--amazon-s3-basedir)
  (setq easy-jekyll-url easy-jekyll--amazon-s3-url)
  (setq easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name))

;;;###autoload
(defun easy-jekyll-google-cloud-storage-deploy ()
  "Deploy jekyll source at Google Cloud Storage."
  (interactive)
  (easy-jekyll-with-env
   (unless (executable-find "gsutil")
     (error "'Google Cloud SDK' is not installed"))
   (unless easy-jekyll-google-cloud-storage-bucket-name
     (error "Please set 'easy-jekyll-google-cloud-storage-bucket-name' variable"))
   (when (file-directory-p "public")
     (delete-directory "public" t nil))
   (shell-command-to-string "jekyll --destination public")
   (shell-command-to-string (concat "gsutil -m rsync -d -r public gs://" easy-jekyll-google-cloud-storage-bucket-name "/"))
   (message "Blog deployed")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-google-cloud-storage-deploy-timer(n)
  "A timer that google-cloud-storage-deploy after the specified number of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--google-cloud-storage-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--google-cloud-storage-url-timer easy-jekyll-url)
  (setq easy-jekyll--google-cloud-storage-bucket-name-timer easy-jekyll-google-cloud-storage-bucket-name)
  (setq easy-jekyll--google-cloud-storage-timer
	(run-at-time (* n 60) nil #'easy-jekyll-google-cloud-storage-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-google-cloud-storage-deploy-timer()
  "Cancel timer that google-cloud-storage-deploy after the specified number of minutes has elapsed."
  (interactive)
  (when easy-jekyll--google-cloud-storage-timer
    (cancel-timer easy-jekyll--google-cloud-storage-timer)
    (setq easy-jekyll--google-cloud-storage-timer nil)
    (message "Easy-jekyll-google-cloud-storage-deploy-timer canceled")))

(defun easy-jekyll-google-cloud-storage-deploy-on-timer ()
  "Deploy jekyll source at Google Cloud Storage on timer."
  (setq easy-jekyll--google-cloud-storage-basedir easy-jekyll-basedir)
  (setq easy-jekyll-basedir easy-jekyll--google-cloud-storage-basedir-timer)
  (setq easy-jekyll--google-cloud-storage-url easy-jekyll-url)
  (setq easy-jekyll-url easy-jekyll--google-cloud-storage-url-timer)
  (setq easy-jekyll--google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name)
  (setq easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-timer)
  (easy-jekyll-google-cloud-storage-deploy)
  (setq easy-jekyll-basedir easy-jekyll--google-cloud-storage-basedir)
  (setq easy-jekyll-url easy-jekyll--google-cloud-storage-url)
  (setq easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name))

;;;###autoload
(defun easy-jekyll-helm-ag ()
  "Search for blog article with helm-ag."
  (interactive)
  (easy-jekyll-with-env
   (if (package-installed-p 'helm-ag)
       (helm-ag (expand-file-name easy-jekyll-postdir easy-jekyll-basedir))
     (error "'helm-ag' is not installed"))))

;;;###autoload
(defun easy-jekyll-open-config ()
  "Open Jekyll's config file."
  (interactive)
  (easy-jekyll-with-env
   (cond ((file-exists-p (expand-file-name "config.toml" easy-jekyll-basedir))
	  (find-file (expand-file-name "config.toml" easy-jekyll-basedir)))
	 ((file-exists-p (expand-file-name "config.yaml" easy-jekyll-basedir))
	  (find-file (expand-file-name "config.yaml" easy-jekyll-basedir)))
	 ((file-exists-p (expand-file-name "config.json" easy-jekyll-basedir))
	  (find-file (expand-file-name "config.json" easy-jekyll-basedir)))
	 (t (error "Jekyll config file not found at %s" easy-jekyll-basedir)))))

(defconst easy-jekyll--help
  (if (null easy-jekyll-sort-default-char)
      (progn
	"n .. New blog post    R .. Rename file   G .. Deploy GitHub    D .. Draft list
p .. Preview          g .. Refresh       A .. Deploy AWS S3    u .. Undraft file
v .. Open view-mode   s .. Sort time     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   ? .. Help easy-jekyll   I .. Deploy GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. Deploy GitHub timer
< .. Previous blog    > .. Next blog     q .. Quit easy-jekyll   W .. Deploy AWS S3 timer

")
    (progn
      "n .. New blog post    R .. Rename file   G .. Deploy GitHub    D .. Draft list
p .. Preview          g .. Refresh       A .. Deploy AWS S3    s .. Sort character
v .. Open view-mode   u .. Undraft file  T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   ? .. Help easy-jekyll   I .. Deploy GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. Deploy GitHub timer
< .. Previous blog    > .. Next blog     q .. Quit easy-jekyll   W .. Deploy AWS S3 timer

"))
  "Help of easy-jekyll.")

(defconst easy-jekyll--first-help
  "Welcome to Easy-jekyll

Let's post an article first.
Press n on this screen or M-x easy-jekyll-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-jekyll again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!

"
  "Help of easy-jekyll first time.")

(defvar easy-jekyll-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'easy-jekyll-newpost)
    (define-key map "w" 'easy-jekyll-newpost)
    (define-key map "a" 'easy-jekyll-helm-ag)
    (define-key map "c" 'easy-jekyll-open-config)
    (define-key map "p" 'easy-jekyll-preview)
    (define-key map "P" 'easy-jekyll-publish)
    (define-key map "T" 'easy-jekyll-publish-timer)
    (define-key map "W" 'easy-jekyll-amazon-s3-deploy-timer)
    (define-key map "t" 'easy-jekyll-cancel-publish-timer)
    (define-key map "o" 'easy-jekyll-open)
    (define-key map "O" 'easy-jekyll-open-basedir)
    (define-key map "R" 'easy-jekyll-rename)
    (define-key map "\C-m" 'easy-jekyll-open)
    (put 'easy-jekyll-open :advertised-binding "\C-m")
    (define-key map "d" 'easy-jekyll-delete)
    (define-key map "e" 'easy-jekyll-open)
    (define-key map "f" 'easy-jekyll-open)
    (define-key map "N" 'easy-jekyll-no-help)
    (define-key map "j" 'easy-jekyll-next-line)
    (define-key map "k" 'easy-jekyll-previous-line)
    (define-key map "h" 'easy-jekyll-backward-char)
    (define-key map "l" 'easy-jekyll-forward-char)
    (define-key map " " 'easy-jekyll-next-line)
    (define-key map [?\S-\ ] 'easy-jekyll-previous-line)
    (define-key map [remap next-line] 'easy-jekyll-next-line)
    (define-key map [remap previous-line] 'easy-jekyll-previous-line)
    (define-key map [remap forward-char] 'easy-jekyll-forward-char)
    (define-key map [remap backward-char] 'easy-jekyll-backward-char)
    (define-key map [remap beginning-of-buffer] 'easy-jekyll-beginning-of-buffer)
    (define-key map [remap backward-word] 'easy-jekyll-backward-word)
    (define-key map [right] 'easy-jekyll-forward-char)
    (define-key map [left] 'easy-jekyll-backward-char)
    (define-key map "v" 'easy-jekyll-view)
    (define-key map "r" 'easy-jekyll-refresh)
    (define-key map "g" 'easy-jekyll-refresh)
    (if (null easy-jekyll-sort-default-char)
	(progn
	  (define-key map "s" 'easy-jekyll-sort-time)
	  (define-key map "S" 'easy-jekyll-sort-char))
      (progn
	(define-key map "S" 'easy-jekyll-sort-time)
	(define-key map "s" 'easy-jekyll-sort-char)))
    (define-key map "G" 'easy-jekyll-github-deploy)
    (define-key map "H" 'easy-jekyll-github-deploy-timer)
    (define-key map "b" 'easy-jekyll-cancel-github-deploy-timer)
    (define-key map "A" 'easy-jekyll-amazon-s3-deploy)
    (define-key map "m" 'easy-jekyll-cancel-amazon-s3-deploy-timer)
    (define-key map "C" 'easy-jekyll-google-cloud-storage-deploy)
    (define-key map "I" 'easy-jekyll-google-cloud-storage-deploy-timer)
    (define-key map "i" 'easy-jekyll-cancel-google-cloud-storage-deploy-timer)
    (define-key map "D" 'easy-jekyll-list-draft)
    (define-key map "u" 'easy-jekyll-undraft)
    (define-key map "q" 'easy-jekyll-quit)
    (define-key map "<" 'easy-jekyll-previous-blog)
    (define-key map ">" 'easy-jekyll-next-blog)
    map)
  "Keymap for easy-jekyll major mode.")

(defvar easy-jekyll--mode-buffer nil
  "Main buffer of easy-jekyll.")

(defvar easy-jekyll--cursor nil
  "Cursor of easy-jekyll.")

(defvar easy-jekyll--line nil
  "Line of easy-jekyll.")

(defvar easy-jekyll--sort-time-flg 1
  "Sort time flg of easy-jekyll.")

(defvar easy-jekyll--sort-char-flg nil
  "Sort char flg of easy-jekyll.")

(defvar easy-jekyll--refresh nil
  "Refresh flg of easy-jekyll.")

(defvar easy-jekyll--current-blog 0
  "Current blog number.")

(defconst easy-jekyll--blog-maximum-number 10
  "Maximum number of blogs.")

(defconst easy-jekyll--basedir-0 easy-jekyll-basedir
  "Default blog base directory.")

(defconst easy-jekyll--url-0 easy-jekyll-url
  "Default blog url.")

(defconst easy-jekyll--root-0 easy-jekyll-root
  "Default blog root.")

(defconst easy-jekyll--sshdomain-0 easy-jekyll-sshdomain
  "Default blog sshdomain.")

(defconst easy-jekyll--amazon-s3-bucket-name-0 easy-jekyll-amazon-s3-bucket-name
  "Default blog amazon s3 bucket name.")

(defconst easy-jekyll--google-cloud-storage-bucket-name-0 easy-jekyll-google-cloud-storage-bucket-name
  "Default blog google cloud storage bucket name.")

(defconst easy-jekyll--buffer-name "*Easy-jekyll*"
  "Buffer name of easy-jekyll.")

(defconst easy-jekyll--forward-char 20
  "Forward-char of easy-jekyll.")

(define-derived-mode easy-jekyll-mode special-mode "Easy-jekyll"
  "Major mode for easy jekyll.")

(defun easy-jekyll-quit ()
  "Quit easy jekyll."
  (interactive)
  (setq easy-jekyll--sort-time-flg 1)
  (setq easy-jekyll--sort-char-flg nil)
  (easy-jekyll--preview-end)
  (when (buffer-live-p easy-jekyll--mode-buffer)
    (kill-buffer easy-jekyll--mode-buffer)))

(defun easy-jekyll-no-help ()
  "No help easy jekyll."
  (interactive)
  (if easy-jekyll-no-help
      (progn
	(setq easy-jekyll-no-help nil)
	(setq easy-jekyll--unmovable-line easy-jekyll--unmovable-line-default))
    (progn
      (setq easy-jekyll-no-help 1)
      (setq easy-jekyll--unmovable-line 3)))
  (if easy-jekyll--draft-list
      (easy-jekyll-draft-list)
    (easy-jekyll)))

(defun easy-jekyll-list-draft ()
  "List drafts."
  (interactive)
  (if easy-jekyll--draft-list
      (progn
	(setq easy-jekyll--draft-list nil)
	(setq easy-jekyll--draft-mode nil)
	(easy-jekyll))
    (progn
      (setq easy-jekyll--draft-list 1)
      (setq easy-jekyll--draft-mode "  Draft")
      (easy-jekyll-draft-list))))

(defun easy-jekyll-refresh ()
  "Refresh easy jekyll."
  (interactive)
  (setq easy-jekyll--cursor (point))
  (setq easy-jekyll--refresh 1)
  (if easy-jekyll--draft-list
      (easy-jekyll-draft-list)
    (easy-jekyll))
  (setq easy-jekyll--refresh nil))

(defun easy-jekyll-sort-time ()
  "Sort time easy jekyll."
  (interactive)
  (if easy-jekyll--draft-list
      (progn
	(setq easy-jekyll--sort-char-flg nil)
	(if (eq 1 easy-jekyll--sort-time-flg)
	    (setq easy-jekyll--sort-time-flg 2)
	  (setq easy-jekyll--sort-time-flg 1))
	(easy-jekyll-draft-list))
    (progn
      (setq easy-jekyll--sort-char-flg nil)
      (if (eq 1 easy-jekyll--sort-time-flg)
	  (setq easy-jekyll--sort-time-flg 2)
	(setq easy-jekyll--sort-time-flg 1))
      (easy-jekyll))))

(defun easy-jekyll-sort-char ()
  "Sort char easy jekyll."
  (interactive)
  (if easy-jekyll--draft-list
      (progn
	(setq easy-jekyll--sort-time-flg nil)
	(if (eq 1 easy-jekyll--sort-char-flg)
	    (setq easy-jekyll--sort-char-flg 2)
	  (setq easy-jekyll--sort-char-flg 1))
	(easy-jekyll-draft-list))
    (progn
      (setq easy-jekyll--sort-time-flg nil)
      (if (eq 1 easy-jekyll--sort-char-flg)
	  (setq easy-jekyll--sort-char-flg 2)
	(setq easy-jekyll--sort-char-flg 1))
      (easy-jekyll))))

(defun easy-jekyll-forward-char (arg)
  "Forward-char as ARG."
  (interactive "^p")
  (when (not (eolp))
    (forward-char (or arg 1))))

(defun easy-jekyll-backward-char (arg)
  "Backward-char as ARG."
  (interactive "^p")
  (when (not (bolp))
    (backward-char (or arg 1))))

(defun easy-jekyll-beginning-of-buffer ()
  "Easy-jekyll beginning-of-buffer."
  (interactive)
  (goto-char (point-min))
  (forward-line (- easy-jekyll--unmovable-line 1))
  (forward-char easy-jekyll--forward-char))

(defun easy-jekyll-backward-word (&optional arg)
  "Easy-jekyll backward-word as ARG."
  (interactive "^p")
  (forward-word (- (or arg 1)))
  (if (< (line-number-at-pos) easy-jekyll--unmovable-line)
      (progn
	(goto-char (point-min))
	(forward-line (- easy-jekyll--unmovable-line 1)))))

(defun easy-jekyll-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (let ((line-move-visual)
	(goal-column))
    (line-move arg t))
  (while (and (invisible-p (point))
	      (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (beginning-of-line)
  (forward-char easy-jekyll--forward-char))

(defun easy-jekyll-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (when (>= (- (line-number-at-pos) arg) easy-jekyll--unmovable-line)
    (easy-jekyll-next-line (- (or arg 1)))))

(defun easy-jekyll-rename (post-file)
  "Renames file on the pointer to POST-FILE."
  (interactive (list (read-from-minibuffer "Rename: " `(,easy-jekyll-default-ext . 1) nil nil nil)))
  (let ((filename (concat (replace-regexp-in-string (regexp-quote "content/") "" easy-jekyll-postdir t t) "/" post-file))
        (file-ext (file-name-extension post-file)))
    (when (not (member file-ext easy-jekyll--formats))
      (error "Please enter .%s or .org or .%s or .rst or .mmark or .%s file name" easy-jekyll-markdown-extension easy-jekyll-asciidoc-extension easy-jekyll-html-extension))
    (easy-jekyll-with-env
     (when (file-exists-p (file-truename (concat "content/" filename)))
       (error "%s already exists!" (concat easy-jekyll-basedir "content/" filename)))
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
       (let ((name (expand-file-name
		    (concat easy-jekyll-postdir "/" (substring (thing-at-point 'line) easy-jekyll--forward-char -1))
		    easy-jekyll-basedir)))
	 (rename-file name (concat "content/" filename) 1)
	 (easy-jekyll-refresh))))))

(defun easy-jekyll-undraft ()
  "Undraft file on the pointer."
  (interactive)
  (easy-jekyll-with-env
   (when (> 0.25 (easy-jekyll--version))
     (error "'easy-jekyll-undraft' requires jekyll 0.25 or higher"))
   (unless (or (string-match "^$" (thing-at-point 'line))
	       (eq (point) (point-max))
	       (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
     (let ((file (expand-file-name
		  (concat easy-jekyll-postdir "/" (substring (thing-at-point 'line) easy-jekyll--forward-char -1))
		  easy-jekyll-basedir)))
       (when (and (file-exists-p file)
		  (not (file-directory-p file)))
	 (shell-command-to-string (concat "jekyll undraft " file))
	 (easy-jekyll-refresh))))))

(defun easy-jekyll-open ()
  "Open the file on the pointer."
  (interactive)
  (unless (or (string-match "^$" (thing-at-point 'line))
	      (eq (point) (point-max))
	      (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
    (let ((file (expand-file-name
		 (concat easy-jekyll-postdir "/" (substring (thing-at-point 'line) easy-jekyll--forward-char -1))
		 easy-jekyll-basedir)))
      (when (and (file-exists-p file)
		 (not (file-directory-p file)))
	(find-file file)))))

(defun easy-jekyll-open-basedir ()
  "Open `easy-jekyll-basedir' with dired."
  (interactive)
  (easy-jekyll-with-env
   (switch-to-buffer (find-file-noselect easy-jekyll-basedir))))

(defun easy-jekyll-view ()
  "Open the file on the pointer with 'view-mode'."
  (interactive)
  (unless (or (string-match "^$" (thing-at-point 'line))
	      (eq (point) (point-max))
	      (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
    (let ((file (expand-file-name
		 (concat easy-jekyll-postdir "/" (substring (thing-at-point 'line) easy-jekyll--forward-char -1))
		 easy-jekyll-basedir)))
      (when (and (file-exists-p file)
		 (not (file-directory-p file)))
	(view-file file)))))

(defun easy-jekyll-delete ()
  "Delete the file on the pointer."
  (interactive)
  (unless (or (string-match "^$" (thing-at-point 'line))
	      (eq (point) (point-max))
	      (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
    (let ((file (expand-file-name
		 (concat easy-jekyll-postdir "/" (substring (thing-at-point 'line) easy-jekyll--forward-char -1))
		 easy-jekyll-basedir)))
      (when (and (file-exists-p file)
		 (not (file-directory-p file)))
	(when (y-or-n-p (concat "Delete " file))
	  (if easy-jekyll-no-help
	      (setq easy-jekyll--line (- (line-number-at-pos) 4))
	    (setq easy-jekyll--line (- (line-number-at-pos) easy-jekyll--delete-line)))
	  (delete-file file)
	  (if easy-jekyll--draft-list
	      (easy-jekyll-draft-list)
	    (easy-jekyll))
	  (when (> easy-jekyll--line 0)
	    (forward-line easy-jekyll--line)
	    (forward-char easy-jekyll--forward-char)))))))

(defun easy-jekyll-next-blog ()
  "Go to next blog."
  (interactive)
  (if (eq easy-jekyll--blog-maximum-number easy-jekyll--current-blog)
      (setq easy-jekyll--current-blog 0)
    (setq easy-jekyll--current-blog (+ easy-jekyll--current-blog 1)))
  (cond ((eq easy-jekyll--current-blog 1) (easy-jekyll-1))
	((eq easy-jekyll--current-blog 2) (easy-jekyll-2))
	((eq easy-jekyll--current-blog 3) (easy-jekyll-3))
	((eq easy-jekyll--current-blog 4) (easy-jekyll-4))
	((eq easy-jekyll--current-blog 5) (easy-jekyll-5))
	((eq easy-jekyll--current-blog 6) (easy-jekyll-6))
	((eq easy-jekyll--current-blog 7) (easy-jekyll-7))
	((eq easy-jekyll--current-blog 8) (easy-jekyll-8))
	((eq easy-jekyll--current-blog 9) (easy-jekyll-9))
	((eq easy-jekyll--current-blog 0) (easy-jekyll-0))))

(defun easy-jekyll-previous-blog ()
  "Go to previous blog."
  (interactive)
  (if (> 0 (- easy-jekyll--current-blog 1))
      (when easy-jekyll-blog-number
	(setq easy-jekyll--current-blog (- easy-jekyll-blog-number 1)))
    (setq easy-jekyll--current-blog (- easy-jekyll--current-blog 1)))
  (cond ((eq easy-jekyll--current-blog 1) (easy-jekyll-1))
	((eq easy-jekyll--current-blog 2) (easy-jekyll-2))
	((eq easy-jekyll--current-blog 3) (easy-jekyll-3))
	((eq easy-jekyll--current-blog 4) (easy-jekyll-4))
	((eq easy-jekyll--current-blog 5) (easy-jekyll-5))
	((eq easy-jekyll--current-blog 6) (easy-jekyll-6))
	((eq easy-jekyll--current-blog 7) (easy-jekyll-7))
	((eq easy-jekyll--current-blog 8) (easy-jekyll-8))
	((eq easy-jekyll--current-blog 9) (easy-jekyll-9))
	((eq easy-jekyll--current-blog 0) (easy-jekyll-0))))

(defun easy-jekyll-0 ()
  "Default blog."
  (interactive)
  (setq easy-jekyll-basedir easy-jekyll--basedir-0
        easy-jekyll-url easy-jekyll--url-0
        easy-jekyll-root easy-jekyll--root-0
	easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
  (easy-jekyll--preview-end)
  (easy-jekyll))

(defun easy-jekyll-1 ()
  "Blog1."
  (interactive)
  (if (or (null easy-jekyll-basedir-1) (null easy-jekyll-url-1))
      (setq easy-jekyll--current-blog 0)
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-1
	    easy-jekyll-url easy-jekyll-url-1
	    easy-jekyll-root easy-jekyll-root-1
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-1
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-1
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-1)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-2 ()
  "Blog2."
  (interactive)
  (if (or (null easy-jekyll-basedir-2) (null easy-jekyll-url-2))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-2
	    easy-jekyll-url easy-jekyll-url-2
	    easy-jekyll-root easy-jekyll-root-2
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-2
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-2
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-2)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-3 ()
  "Blog3."
  (interactive)
  (if (or (null easy-jekyll-basedir-3) (null easy-jekyll-url-3))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-3
	    easy-jekyll-url easy-jekyll-url-3
	    easy-jekyll-root easy-jekyll-root-3
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-3
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-3
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-3)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-4 ()
  "Blog4."
  (interactive)
  (if (or (null easy-jekyll-basedir-4) (null easy-jekyll-url-4))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-4
	    easy-jekyll-url easy-jekyll-url-4
	    easy-jekyll-root easy-jekyll-root-4
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-4
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-4
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-4)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-5 ()
  "Blog5."
  (interactive)
  (if (or (null easy-jekyll-basedir-5) (null easy-jekyll-url-5))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-5
	    easy-jekyll-url easy-jekyll-url-5
	    easy-jekyll-root easy-jekyll-root-5
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-5
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-5
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-5)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-6 ()
  "Blog6."
  (interactive)
  (if (or (null easy-jekyll-basedir-6) (null easy-jekyll-url-6))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-6
	    easy-jekyll-url easy-jekyll-url-6
	    easy-jekyll-root easy-jekyll-root-6
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-6
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-6
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-6)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-7 ()
  "Blog7."
  (interactive)
  (if (or (null easy-jekyll-basedir-7) (null easy-jekyll-url-7))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-7
	    easy-jekyll-url easy-jekyll-url-7
	    easy-jekyll-root easy-jekyll-root-7
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-7
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-7
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-7)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-8 ()
  "Blog8."
  (interactive)
  (if (or (null easy-jekyll-basedir-8) (null easy-jekyll-url-8))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-8
	    easy-jekyll-url easy-jekyll-url-8
	    easy-jekyll-root easy-jekyll-root-8
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-8
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-8
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-8)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-9 ()
  "Blog9."
  (interactive)
  (if (or (null easy-jekyll-basedir-9) (null easy-jekyll-url-9))
      (progn
	(setq easy-jekyll--current-blog 0)
	(setq easy-jekyll-basedir easy-jekyll--basedir-0
	      easy-jekyll-url easy-jekyll--url-0
	      easy-jekyll-root easy-jekyll--root-0
	      easy-jekyll-sshdomain easy-jekyll--sshdomain-0
	      easy-jekyll-amazon-s3-bucket-name easy-jekyll--amazon-s3-bucket-name-0
	      easy-jekyll-google-cloud-storage-bucket-name easy-jekyll--google-cloud-storage-bucket-name-0)
	(easy-jekyll--preview-end)
	(easy-jekyll))
    (progn
      (setq easy-jekyll-basedir easy-jekyll-basedir-9
	    easy-jekyll-url easy-jekyll-url-9
	    easy-jekyll-root easy-jekyll-root-9
	    easy-jekyll-sshdomain easy-jekyll-sshdomain-9
	    easy-jekyll-amazon-s3-bucket-name easy-jekyll-amazon-s3-bucket-name-9
	    easy-jekyll-google-cloud-storage-bucket-name easy-jekyll-google-cloud-storage-bucket-name-9)
      (easy-jekyll--preview-end)
      (easy-jekyll))))

(defun easy-jekyll-draft-list ()
  "List drafts."
  (easy-jekyll-with-env
   (when (> 0.25 (easy-jekyll--version))
     (error "'List draft' requires jekyll 0.25 or higher"))
   (let ((source (split-string
		  (with-temp-buffer
		    (let ((ret (call-process-shell-command "jekyll list drafts" nil t)))
		      (unless (zerop ret)
			(error "'Jekyll list drafts' comaand does not end normally"))
		      (buffer-string)))
		  "\n"))
	 (lists (list))
	 (files (list)))
     (dolist (file source)
       (when (string-match ".*/\\(.+?\\)$" file)
	 (push (match-string 1 file) files)))
     (unless (file-directory-p (expand-file-name easy-jekyll-postdir easy-jekyll-basedir))
       (error "Did you execute jekyll new site bookshelf?"))
     (setq easy-jekyll--mode-buffer (get-buffer-create easy-jekyll--buffer-name))
     (switch-to-buffer easy-jekyll--mode-buffer)
     (setq-local default-directory easy-jekyll-basedir)
     (setq buffer-read-only nil)
     (erase-buffer)
     (insert (propertize (concat "Easy-jekyll  " easy-jekyll-url easy-jekyll--draft-mode "\n\n") 'face 'easy-jekyll-help-face))
     (unless easy-jekyll-no-help
       (insert (propertize easy-jekyll--help 'face 'easy-jekyll-help-face)))
     (unless easy-jekyll--refresh
       (setq easy-jekyll--cursor (point)))
     (cond ((eq 1 easy-jekyll--sort-char-flg) (setq files (reverse (sort files 'string<))))
	   ((eq 2 easy-jekyll--sort-char-flg) (setq files (sort files 'string<))))
     (while files
       (push
	(concat
	 (format-time-string "%Y-%m-%d %H:%M:%S " (nth 5 (file-attributes
							  (expand-file-name
							   (concat easy-jekyll-postdir "/" (car files))
							   easy-jekyll-basedir))))
	 (car files))
	lists)
       (pop files))
     (cond ((eq 1 easy-jekyll--sort-time-flg) (setq lists (reverse (sort lists 'string<))))
	   ((eq 2 easy-jekyll--sort-time-flg) (setq lists (sort lists 'string<))))
     (while lists
       (insert (concat (car lists) "\n"))
       (pop lists))
     (goto-char easy-jekyll--cursor)
     (if easy-jekyll--refresh
	 (progn
	   (when (< (line-number-at-pos) easy-jekyll--unmovable-line)
	     (goto-char (point-min))
	     (forward-line (- easy-jekyll--unmovable-line 1)))
	   (beginning-of-line)
	   (forward-char easy-jekyll--forward-char))
       (forward-char easy-jekyll--forward-char))
     (easy-jekyll-mode))))

;;;###autoload
(defun easy-jekyll ()
  "Easy jekyll."
  (interactive)
  (easy-jekyll-with-env
   (unless (file-directory-p (expand-file-name easy-jekyll-postdir easy-jekyll-basedir))
     (error "Did you execute jekyll new site bookshelf?"))
   (setq easy-jekyll--mode-buffer (get-buffer-create easy-jekyll--buffer-name))
   (setq easy-jekyll--draft-list nil)
   (switch-to-buffer easy-jekyll--mode-buffer)
   (setq-local default-directory easy-jekyll-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert (propertize (concat "Easy-jekyll  " easy-jekyll-url "\n\n") 'face 'easy-jekyll-help-face))
   (unless easy-jekyll-no-help
     (insert (propertize easy-jekyll--help 'face 'easy-jekyll-help-face)))
   (unless easy-jekyll--refresh
     (setq easy-jekyll--cursor (point)))
   (let ((files (directory-files (expand-file-name easy-jekyll-postdir easy-jekyll-basedir)))
	 (lists (list)))
     (if (eq 2 (length files))
	 (progn
	   (insert easy-jekyll--first-help)
	   (easy-jekyll-mode)
	   (goto-char easy-jekyll--cursor))
       (progn
	 (cond ((eq 1 easy-jekyll--sort-char-flg) (setq files (reverse (sort files 'string<))))
	       ((eq 2 easy-jekyll--sort-char-flg) (setq files (sort files 'string<))))
	 (while files
	   (unless (or (string= (car files) ".")
		       (string= (car files) ".."))
	     (push
	      (concat
	       (format-time-string "%Y-%m-%d %H:%M:%S " (nth 5 (file-attributes
								(expand-file-name
								 (concat easy-jekyll-postdir "/" (car files))
								 easy-jekyll-basedir))))
	       (car files))
	      lists))
	   (pop files))
	 (cond ((eq 1 easy-jekyll--sort-time-flg) (setq lists (reverse (sort lists 'string<))))
	       ((eq 2 easy-jekyll--sort-time-flg) (setq lists (sort lists 'string<))))
	 (while lists
	   (insert (concat (car lists) "\n"))
	   (pop lists))
	 (goto-char easy-jekyll--cursor)
	 (if easy-jekyll--refresh
	     (progn
	       (when (< (line-number-at-pos) easy-jekyll--unmovable-line)
		 (goto-char (point-min))
		 (forward-line (- easy-jekyll--unmovable-line 1)))
	       (beginning-of-line)
	       (forward-char easy-jekyll--forward-char))
	   (forward-char easy-jekyll--forward-char))
	 (easy-jekyll-mode))))))

(provide 'easy-jekyll)

;;; easy-jekyll.el ends here
