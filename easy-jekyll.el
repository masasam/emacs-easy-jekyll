;;; easy-jekyll.el --- Major mode managing jekyll blogs -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-easy-jekyll
;; Version: 0.9.9
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

(defcustom easy-jekyll-image-directory "images"
  "Image file directory under 'static' directory."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-default-picture-directory "~"
  "Default directory for selecting images with `easy-jekyll-put-image'."
  :group 'easy-jekyll
  :type 'string)

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

(defcustom easy-jekyll-sort-default-char t
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

(defcustom easy-jekyll-postdir "_posts"
  "Directory where stores its posts."
  :group 'easy-jekyll
  :type 'string)

(defcustom easy-jekyll-blog-number nil
  "Number of blogs you want to manage."
  :group 'easy-jekyll
  :type 'integer)

(defvar easy-jekyll--current-postdir 0
  "Easy-jekyll current postdir.")

(defvar easy-jekyll--postdir-list nil
  "Easy-jekyll postdir list.")

(defvar easy-jekyll--preview-loop t
  "Preview loop flg.")

(defvar easy-jekyll--server-process nil
  "Jekyll process.")

(defvar easy-jekyll--unmovable-line 11
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

(defconst easy-jekyll--delete-line 12
  "Easy-jekyll-delete line number.")

(defconst easy-jekyll--buffer-name "*Jekyll Serve*"
  "Easy-jekyll buffer name.")

(defconst easy-jekyll--preview-buffer "*Jekyll Preview*"
  "Easy-jekyll preview buffer name.")

(defconst easy-jekyll--formats `(,easy-jekyll-markdown-extension
				 ,easy-jekyll-textile-extension))

(defface easy-jekyll-help-face
  '((((class color) (background light)) (:inherit font-lock-function-name-face :background "#f0f8ff"))
    (((class color) (background dark)) (:inherit font-lock-function-name-face :background "#2f4f4f")))
  ""
  :group 'easy-jekyll-faces)

(defvar easy-jekyll--mode-buffer nil
  "Main buffer of easy-jekyll.")

(defvar easy-jekyll--cursor nil
  "Cursor of easy-jekyll.")

(defvar easy-jekyll--line nil
  "Line of easy-jekyll.")

(defvar easy-jekyll--sort-time-flg nil
  "Sort time flg of easy-jekyll.")

(defvar easy-jekyll--sort-char-flg 2
  "Sort char flg of easy-jekyll.")

(defvar easy-jekyll--refresh nil
  "Refresh flg of easy-jekyll.")

(defvar easy-jekyll--current-blog 0
  "Current blog number.")

(defconst easy-jekyll--blog-maximum-number 10
  "Maximum number of blogs.")

(defcustom easy-jekyll-bloglist nil
  "Multiple blog setting."
  :group 'easy-jekyll
  :type 'string)

(push `((easy-jekyll-basedir . ,easy-jekyll-basedir)
	(easy-jekyll-url . ,easy-jekyll-url)
	(easy-jekyll-root . ,easy-jekyll-root)
	(easy-jekyll-sshdomain . ,easy-jekyll-sshdomain)
	(easy-jekyll-amazon-s3-bucket-name . ,easy-jekyll-amazon-s3-bucket-name)
	(easy-jekyll-google-cloud-storage-bucket-name . ,easy-jekyll-google-cloud-storage-bucket-name)
	(easy-jekyll-github-deploy-script . ,easy-jekyll-github-deploy-script)
	(easy-jekyll-image-directory . ,easy-jekyll-image-directory)
	(easy-jekyll-default-picture-directory . ,easy-jekyll-default-picture-directory)
	(easy-jekyll-publish-chmod . ,easy-jekyll-publish-chmod))
      easy-jekyll-bloglist)

(defconst easy-jekyll--buffer-name "*Easy-jekyll*"
  "Buffer name of easy-jekyll.")

(defconst easy-jekyll--forward-char 20
  "Forward-char of easy-jekyll.")

(defconst easy-jekyll--default-postdir easy-jekyll-postdir
  "Default easy-jekyll-postdir.")

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
(defun easy-jekyll-image ()
  "Generate image link."
  (interactive
   (easy-jekyll-with-env
    (unless (file-directory-p (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir))
      (error "%s does not exist" (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir)))
    (let ((file (read-file-name "Image file: " nil
				(expand-file-name
				 easy-jekyll-image-directory easy-jekyll-basedir)
				t
				(expand-file-name
				 easy-jekyll-image-directory easy-jekyll-basedir))))
      (insert (concat (format "<img src=\"%s%s\""
			      easy-jekyll-url
			      (concat "/" easy-jekyll-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

;;;###autoload
(defun easy-jekyll-put-image ()
  "Move image to image directory and generate image link."
  (interactive
   (easy-jekyll-with-env
    (unless (file-directory-p (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir))
      (error "%s does not exist" (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir)))
    (let ((file (read-file-name "Image file: " nil
				(expand-file-name easy-jekyll-default-picture-directory)
				t
				(expand-file-name easy-jekyll-default-picture-directory))))
      (copy-file file (expand-file-name (file-name-nondirectory file) easy-jekyll-image-directory))
      (insert (concat (format "<img src=\"%s%s\""
			      easy-jekyll-url
			      (concat "/" easy-jekyll-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

;;;###autoload
(defun easy-jekyll-pull-image ()
  "Pull image from internet to image directory and generate image link."
  (interactive
   (easy-jekyll-with-env
    (unless (file-directory-p (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir))
      (error "%s does not exist" (expand-file-name easy-jekyll-image-directory easy-jekyll-basedir)))
    (let ((url (read-string "URL: " (if (fboundp 'gui-get-selection) (gui-get-selection))))
	  (file (read-file-name "Save as: "
				(expand-file-name easy-jekyll-image-directory easy-jekyll-basedir)
				(car (last (split-string (substring-no-properties (gui-get-selection)) "/")))
				nil)))
      (when (file-exists-p (file-truename file))
	(error "%s already exists!" (file-truename file)))
      (url-copy-file url file t)
      (insert (concat (format "<img src=\"%s%s\""
			      easy-jekyll-url
			      (concat "/" easy-jekyll-image-directory "/" (file-name-nondirectory file)))
		      " alt=\"\" width=\"100%\"/>"))))))

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
   (let ((ret (call-process "bundle" nil "*jekyll-publish*" t "exec" "jekyll" "build" "--destination" "_site")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*jekyll-publish*"))
       (error "'bundle exec jekyll build' command does not end normally")))
   (when (get-buffer "*jekyll-publish*")
     (kill-buffer "*jekyll-publish*"))
   (shell-command-to-string (concat "rsync -rtpl --chmod=" easy-jekyll-publish-chmod " --delete _site/ " easy-jekyll-sshdomain ":" (shell-quote-argument easy-jekyll-root)))
   (message "Blog published")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-publish-timer (n)
  "A timer that publish after the specified number as N of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--sshdomain-timer easy-jekyll-sshdomain)
  (setq easy-jekyll--root-timer easy-jekyll-root)
  (setq easy-jekyll--url-timer easy-jekyll-url)
  (setq easy-jekyll--publish-timer
	(run-at-time (* n 60) nil #'easy-jekyll-publish-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-publish-timer ()
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
     "\n---\n")))

;;;###autoload
(defun easy-jekyll-newpost (post-file)
  "Create a new post with jekyll.
POST-FILE needs to have and extension '.md' or '.textile'."
  (interactive (list (read-from-minibuffer "Filename: " `(,easy-jekyll-default-ext . 1) nil nil nil)))
  (easy-jekyll-with-env
   (let ((filename (if easy-jekyll--draft-list
		       (expand-file-name post-file "_drafts")
		     (expand-file-name (concat (format-time-string "%Y-%m-%d-" (current-time)) post-file) easy-jekyll-postdir)))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-jekyll--formats))
       (error "Please enter .%s file name or .%s file name" easy-jekyll-markdown-extension easy-jekyll-textile-extension))
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))
     (find-file filename)
     (insert (easy-jekyll--headers (file-name-base post-file)))
     (goto-char (point-max))
     (save-buffer))))

(defun easy-jekyll--version ()
  "Return the version of jekyll."
  (let ((source (split-string
		 (with-temp-buffer
		   (shell-command-to-string "bundle exec jekyll --version"))
		 " ")))
    (string-to-number (nth 1 source))))

;;;###autoload
(defun easy-jekyll-preview ()
  "Preview jekyll at localhost."
  (interactive)
  (easy-jekyll-with-env
   (if (process-live-p easy-jekyll--server-process)
       (browse-url easy-jekyll-preview-url)
     (progn
       (setq easy-jekyll--server-process
	     (start-process "jekyll-serve" easy-jekyll--preview-buffer "bundle" "exec" "jekyll" "serve"))
       (while easy-jekyll--preview-loop
	 (if (equal (easy-jekyll--preview-status) "200")
	     (progn
	       (setq easy-jekyll--preview-loop nil)
	       (browse-url easy-jekyll-preview-url)))
	 (sleep-for 0 100)
	 (if (and (eq (process-status easy-jekyll--server-process) 'exit)
		  (not (equal (process-exit-status easy-jekyll--server-process) 0)))
	     (progn
	       (switch-to-buffer easy-jekyll--preview-buffer)
	       (error "Jekyll error look at %s buffer" easy-jekyll--preview-buffer))))
       (setq easy-jekyll--preview-loop t)
       (run-at-time easy-jekyll-previewtime nil 'easy-jekyll--preview-end)))))

(defun easy-jekyll--preview-status ()
  "Return the http status code of the preview."
  (nth 1
       (split-string
	(nth 0
	     (split-string
	      (with-current-buffer (url-retrieve-synchronously "http://127.0.0.1:4000/")
		(prog1
		    (buffer-string)
		  (kill-buffer)))
	      "\n"))
	" ")))

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
   (let ((deployscript (file-truename (expand-file-name easy-jekyll-github-deploy-script easy-jekyll-basedir))))
     (unless (executable-find deployscript)
       (error "%s do not execute" deployscript))
     (let ((ret (call-process (shell-quote-argument deployscript) nil "*jekyll-github-deploy*" t)))
       (unless (zerop ret)
	 (switch-to-buffer (get-buffer "*jekyll-github-deploy*"))
	 (error "%s command does not end normally" deployscript)))
     (when (get-buffer "*jekyll-github-deploy*")
       (kill-buffer "*jekyll-github-deploy*"))
     (message "Blog deployed")
     (when easy-jekyll-url
       (browse-url easy-jekyll-url)))))

;;;###autoload
(defun easy-jekyll-github-deploy-timer (n)
  "A timer that github-deploy after the specified number as N of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--github-deploy-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--github-deploy-url-timer easy-jekyll-url)
  (setq easy-jekyll--github-deploy-timer
	(run-at-time (* n 60) nil #'easy-jekyll-github-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-github-deploy-timer ()
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
   (when (file-directory-p "_site")
     (delete-directory "_site" t nil))
   (let ((ret (call-process "bundle" nil "*jekyll-amazon-s3-deploy*" t "exec" "jekyll" "build" "--destination" "_site")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*jekyll-amazon-s3-deploy*"))
       (error "'bundle exec jekyll build' command does not end normally")))
   (when (get-buffer "*jekyll-amazon-s3-deploy*")
     (kill-buffer "*jekyll-amazon-s3-deploy*"))
   (shell-command-to-string (concat "aws s3 sync --delete _site s3://" easy-jekyll-amazon-s3-bucket-name "/"))
   (message "Blog deployed")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-amazon-s3-deploy-timer (n)
  "A timer that amazon-s3-deploy after the specified number as N of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--amazon-s3-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--amazon-s3-url-timer easy-jekyll-url)
  (setq easy-jekyll--amazon-s3-bucket-name-timer easy-jekyll-amazon-s3-bucket-name)
  (setq easy-jekyll--amazon-s3-timer
	(run-at-time (* n 60) nil #'easy-jekyll-amazon-s3-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-amazon-s3-deploy-timer ()
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
   (when (file-directory-p "_site")
     (delete-directory "_site" t nil))
   (let ((ret (call-process "bundle" nil "*jekyll-google-cloud-storage-deploy*" t "exec" "jekyll" "build" "--destination" "_site")))
     (unless (zerop ret)
       (switch-to-buffer (get-buffer "*jekyll-google-cloud-storage-deploy*"))
       (error "'bundle exec jekyll build' command does not end normally")))
   (when (get-buffer "*jekyll-google-cloud-storage-deploy*")
     (kill-buffer "*jekyll-google-cloud-storage-deploy*"))
   (shell-command-to-string (concat "gsutil -m rsync -d -r _site gs://" easy-jekyll-google-cloud-storage-bucket-name "/"))
   (message "Blog deployed")
   (when easy-jekyll-url
     (browse-url easy-jekyll-url))))

;;;###autoload
(defun easy-jekyll-google-cloud-storage-deploy-timer (n)
  "A timer that google-cloud-storage-deploy after the specified number as N of minutes has elapsed."
  (interactive "nMinute:")
  (setq easy-jekyll--google-cloud-storage-basedir-timer easy-jekyll-basedir)
  (setq easy-jekyll--google-cloud-storage-url-timer easy-jekyll-url)
  (setq easy-jekyll--google-cloud-storage-bucket-name-timer easy-jekyll-google-cloud-storage-bucket-name)
  (setq easy-jekyll--google-cloud-storage-timer
	(run-at-time (* n 60) nil #'easy-jekyll-google-cloud-storage-deploy-on-timer)))

;;;###autoload
(defun easy-jekyll-cancel-google-cloud-storage-deploy-timer ()
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
   (cond ((file-exists-p (expand-file-name "_config.yml" easy-jekyll-basedir))
	  (find-file (expand-file-name "_config.yml" easy-jekyll-basedir)))
	 (t (error "Jekyll config file not found at %s" easy-jekyll-basedir)))))

(defconst easy-jekyll--help
  (if (null easy-jekyll-sort-default-char)
      (progn
	"n .. New blog post    R .. Rename file   G .. Deploy GitHub    ? .. Help easy-jekyll
p .. Preview          g .. Refresh       A .. Deploy AWS S3    u .. Undraft file
v .. Open view-mode   s .. Sort time     T .. Publish timer    W .. AWS S3 timer
d .. Delete post      c .. Open config   D .. Draft list       I .. GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
O .. Open basedir     S .. Sort char     N .. No help-mode     q .. Quit easy-jekyll

")
    (progn
      "n .. New blog post    R .. Rename file   G .. Deploy GitHub    ? .. Help easy-jekyll
p .. Preview          g .. Refresh       A .. Deploy AWS S3    s .. Sort character
v .. Open view-mode   D .. Draft list    T .. Publish timer    q .. Quit easy-jekyll
d .. Delete post      c .. Open config   u .. Undraft file     I .. GCS timer
P .. Publish server   C .. Deploy GCS    a .. Search helm-ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
O .. Open basedir     W .. AWS S3 timer  N .. No help-mode     q .. Quit easy-jekyll

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
    (define-key map "." 'easy-jekyll-next-postdir)
    (define-key map "," 'easy-jekyll-previous-postdir)
    (define-key map "+" 'easy-jekyll-next-postdir)
    (define-key map "-" 'easy-jekyll-previous-postdir)
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

(define-derived-mode easy-jekyll-mode special-mode "Easy-jekyll"
  "Major mode for easy jekyll.")

(defun easy-jekyll-quit ()
  "Quit easy jekyll."
  (interactive)
  (setq easy-jekyll--sort-time-flg nil)
  (setq easy-jekyll--sort-char-flg 2)
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
  (let ((newname (if easy-jekyll--draft-list
		     (expand-file-name post-file "_drafts")
		   (expand-file-name post-file easy-jekyll-postdir)))
        (file-ext (file-name-extension post-file)))
    (when (not (member file-ext easy-jekyll--formats))
      (error "Please enter .%s file name or .%s file name" easy-jekyll-markdown-extension easy-jekyll-textile-extension))
    (when (equal (buffer-name (current-buffer)) easy-jekyll--buffer-name)
      (easy-jekyll-with-env
       (when (file-exists-p (file-truename newname))
	 (error "%s already exists!" newname))
       (unless (or (string-match "^$" (thing-at-point 'line))
		   (eq (point) (point-max))
		   (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
	 (let ((oldname (if easy-jekyll--draft-list
			    (expand-file-name
			     (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			     "_drafts")
			  (expand-file-name
			   (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			   easy-jekyll-postdir))))
	   (rename-file oldname newname 1)
	   (easy-jekyll-refresh)))))))

(defun easy-jekyll-undraft ()
  "Undraft file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-jekyll--buffer-name)
    (easy-jekyll-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
		    (expand-file-name "_drafts" easy-jekyll-basedir))))
	 (when (and (file-exists-p file)
		    (not (file-directory-p file)))
	   (unless (file-directory-p (expand-file-name "_drafts" easy-jekyll-basedir))
	     (make-directory (expand-file-name "_drafts" easy-jekyll-basedir) t))
	   (rename-file file
			(expand-file-name
			 (concat (format-time-string "%Y-%m-%d-" (current-time))
				 (file-name-nondirectory file))
			 easy-jekyll-postdir)
			1)
	   (easy-jekyll-refresh)))))))

(defun easy-jekyll-open ()
  "Open the file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-jekyll--buffer-name)
    (easy-jekyll-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (if easy-jekyll--draft-list
			(expand-file-name
			 (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			 "_drafts")
		      (expand-file-name
		       (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
		       easy-jekyll-postdir))
		    easy-jekyll-basedir)))
	 (when (and (file-exists-p file)
		    (not (file-directory-p file)))
	   (find-file file)))))))

(defun easy-jekyll-open-basedir ()
  "Open `easy-jekyll-basedir' with dired."
  (interactive)
  (easy-jekyll-with-env
   (switch-to-buffer (find-file-noselect easy-jekyll-basedir))))

(defun easy-jekyll-view ()
  "Open the file on the pointer with 'view-mode'."
  (interactive)
  (easy-jekyll-with-env
   (if (equal (buffer-name (current-buffer)) easy-jekyll--buffer-name)
       (progn
	 (unless (or (string-match "^$" (thing-at-point 'line))
		     (eq (point) (point-max))
		     (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
	   (let ((file (expand-file-name
			(if easy-jekyll--draft-list
			    (expand-file-name
			     (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			     "_drafts")
			  (expand-file-name
			   (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			   easy-jekyll-postdir))
			easy-jekyll-basedir)))
	     (when (and (file-exists-p file)
			(not (file-directory-p file)))
	       (view-file file)))))
     (view-file buffer-file-name))))

(defun easy-jekyll-delete ()
  "Delete the file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) easy-jekyll--buffer-name)
    (easy-jekyll-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
		 (eq (point) (point-max))
		 (> (+ 1 easy-jekyll--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
		    (if easy-jekyll--draft-list
			(expand-file-name
			 (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
			 "_drafts")
		      (expand-file-name
		       (substring (thing-at-point 'line) easy-jekyll--forward-char -1)
		       easy-jekyll-postdir))
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
	       (forward-char easy-jekyll--forward-char)))))))))

(defun easy-jekyll-next-blog ()
  "Go to next blog."
  (interactive)
  (when (< 1 (length easy-jekyll-bloglist))
    (if (eq (- (length easy-jekyll-bloglist) 1) easy-jekyll--current-blog)
	(setq easy-jekyll--current-blog 0)
      (setq easy-jekyll--current-blog (+ easy-jekyll--current-blog 1)))
    (setq easy-jekyll-postdir easy-jekyll--default-postdir)
    (setq easy-jekyll--postdir-list nil)
    (setq easy-jekyll--current-postdir 0)
    (setq easy-jekyll-basedir
	  (cdr (assoc 'easy-jekyll-basedir
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-url
	  (cdr (assoc 'easy-jekyll-url
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-root
	  (cdr (assoc 'easy-jekyll-root
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-sshdomain
	  (cdr (assoc 'easy-jekyll-domain
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-amazon-s3-bucket-name
	  (cdr (assoc 'easy-jekyll-amazon-s3-bucket-name
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-google-cloud-storage-bucket-name
	  (cdr (assoc 'easy-jekyll-google-cloud-storage-bucket-name
		      (nth 1 easy-jekyll-bloglist))))
    (setq easy-jekyll-github-deploy-script
	  (cdr (assoc 'easy-jekyll-github-deploy-script
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-image-directory
	  (cdr (assoc 'easy-jekyll-image-directory
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-default-picture-directory
	  (cdr (assoc 'easy-jekyll-default-picture-directory
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-publish-chmod
	  (cdr (assoc 'easy-jekyll-publish-chmod
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (easy-jekyll--preview-end)
    (easy-jekyll)))

(defun easy-jekyll-previous-blog ()
  "Go to previous blog."
  (interactive)
  (when (< 1 (length easy-jekyll-bloglist))
    (if (= 0 easy-jekyll--current-blog)
	(setq easy-jekyll--current-blog (- (length easy-jekyll-bloglist) 1))
      (setq easy-jekyll--current-blog (- easy-jekyll--current-blog 1)))
    (setq easy-jekyll-postdir easy-jekyll--default-postdir)
    (setq easy-jekyll--postdir-list nil)
    (setq easy-jekyll--current-postdir 0)
    (setq easy-jekyll-basedir
	  (cdr (assoc 'easy-jekyll-basedir
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-url
	  (cdr (assoc 'easy-jekyll-url
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-root
	  (cdr (assoc 'easy-jekyll-root
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-sshdomain
	  (cdr (assoc 'easy-jekyll-domain
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-amazon-s3-bucket-name
	  (cdr (assoc 'easy-jekyll-amazon-s3-bucket-name
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-google-cloud-storage-bucket-name
	  (cdr (assoc 'easy-jekyll-google-cloud-storage-bucket-name
		      (nth 1 easy-jekyll-bloglist))))
    (setq easy-jekyll-github-deploy-script
	  (cdr (assoc 'easy-jekyll-github-deploy-script
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-image-directory
	  (cdr (assoc 'easy-jekyll-image-directory
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-default-picture-directory
	  (cdr (assoc 'easy-jekyll-default-picture-directory
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (setq easy-jekyll-publish-chmod
	  (cdr (assoc 'easy-jekyll-publish-chmod
		      (nth easy-jekyll--current-blog easy-jekyll-bloglist))))
    (easy-jekyll--preview-end)
    (easy-jekyll)))

(defun easy-jekyll-next-postdir ()
  "Go to next postdir."
  (interactive)
  (add-to-list 'easy-jekyll--postdir-list (expand-file-name easy-jekyll-basedir))
  (add-to-list 'easy-jekyll--postdir-list (expand-file-name "_posts" easy-jekyll-basedir))
  (if (eq (- (length easy-jekyll--postdir-list) 1) easy-jekyll--current-postdir)
      (setq easy-jekyll--current-postdir 0)
    (setq easy-jekyll--current-postdir (+ easy-jekyll--current-postdir 1)))
  (setq easy-jekyll-postdir (file-relative-name (nth easy-jekyll--current-postdir easy-jekyll--postdir-list) easy-jekyll-basedir))
  (easy-jekyll))

(defun easy-jekyll-previous-postdir ()
  "Go to previous postdir."
  (interactive)
  (add-to-list 'easy-jekyll--postdir-list (expand-file-name easy-jekyll-basedir))
  (add-to-list 'easy-jekyll--postdir-list (expand-file-name "_posts" easy-jekyll-basedir))
  (setq easy-jekyll--current-postdir (- easy-jekyll--current-postdir 1))
  (when (> 0 easy-jekyll--current-postdir)
    (setq easy-jekyll--current-postdir (- (length easy-jekyll--postdir-list) 1)))
  (setq easy-jekyll-postdir (file-relative-name (nth easy-jekyll--current-postdir easy-jekyll--postdir-list) easy-jekyll-basedir))
  (easy-jekyll))

(defun easy-jekyll-draft-list ()
  "List drafts."
  (easy-jekyll-with-env
   (unless (file-directory-p (expand-file-name "_drafts" easy-jekyll-basedir))
     (make-directory (expand-file-name "_drafts" easy-jekyll-basedir) t))
   (setq easy-jekyll--mode-buffer (get-buffer-create easy-jekyll--buffer-name))
   (setq easy-jekyll--draft-list 1)
   (switch-to-buffer easy-jekyll--mode-buffer)
   (setq-local default-directory easy-jekyll-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (insert (propertize (concat "Easy-jekyll  " easy-jekyll-url easy-jekyll--draft-mode "\n\n") 'face 'easy-jekyll-help-face))
   (unless easy-jekyll-no-help
     (insert (propertize easy-jekyll--help 'face 'easy-jekyll-help-face)))
   (unless easy-jekyll--refresh
     (setq easy-jekyll--cursor (point)))
   (let ((files (directory-files (expand-file-name "_drafts" easy-jekyll-basedir)))
	 (lists (list)))
     (if (eq 2 (length files))
	 (progn
	   (easy-jekyll-mode)
	   (goto-char easy-jekyll--cursor))
       (progn
	 (cond ((eq 1 easy-jekyll--sort-char-flg) (setq files (reverse (sort files 'string<))))
	       ((eq 2 easy-jekyll--sort-char-flg) (setq files (sort files 'string<))))
	 (while files
	   (unless (or (string= (car files) ".")
		       (string= (car files) "..")
		       (not (member (file-name-extension (car files)) easy-jekyll--formats)))
	     (push
	      (concat
	       (format-time-string "%Y-%m-%d %H:%M:%S " (nth 5 (file-attributes
								(expand-file-name
								 (car files)
								 easy-jekyll-postdir))))
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

;;;###autoload
(defun easy-jekyll ()
  "Easy jekyll."
  (interactive)
  (easy-jekyll-with-env
   (unless (file-directory-p (expand-file-name easy-jekyll-postdir easy-jekyll-basedir))
     (error "%s%s doesn't exist!" easy-jekyll-basedir easy-jekyll-postdir))
   (setq easy-jekyll--mode-buffer (get-buffer-create easy-jekyll--buffer-name))
   (setq easy-jekyll--draft-list nil)
   (switch-to-buffer easy-jekyll--mode-buffer)
   (setq-local default-directory easy-jekyll-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (if (equal easy-jekyll-postdir "./")
       (insert (propertize (concat "Easy-jekyll  " easy-jekyll-url "\n\n") 'face 'easy-jekyll-help-face))
     (insert (propertize (concat "Easy-jekyll  " easy-jekyll-url "/" easy-jekyll-postdir "\n\n") 'face 'easy-jekyll-help-face)))
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
		       (string= (car files) "..")
		       (not (member (file-name-extension (car files)) easy-jekyll--formats)))
	     (push
	      (concat
	       (format-time-string "%Y-%m-%d %H:%M:%S " (nth 5 (file-attributes
								(expand-file-name
								 (car files)
								 easy-jekyll-postdir))))
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
