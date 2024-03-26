;;; my-custom-treemacs-theme.el --- Initialize custom treemacs theme   -*- lexical-binding: t -*-
;;
;; Author: Luiz Tagliaferro <luiz@luiznux.com>
;; URL: https://luiznux.com
;; This file is free software :)
;;
;; This package is based on
;; https://github.com/rainstormstudio/treemacs-nerd-icons, but with some
;; modifications.
;;
;; Because treemacs can't handle regex on file extetions, some icons from
;; `nerd-icons-extension-icon-alist' were not displayed correctly from the
;; original package `treemacs-nerd-icons'. Create new treemacs icon for some
;; cases solves this problem.
;;
;; Added new icons for some directories like "src", "main" etc.
;;
;;; Commentary:
;;
;; Custom nerd-icons theme for treemacs.
;;
;;; Code:

(require 'nerd-icons)
(require 'treemacs)

(defface treemacs-nerd-icons-root-face
  '((t (:inherit nerd-icons-dorange)))
  "Face used for the root icon in nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-nerd-icons-file-face
  '((t (:inherit nerd-icons-orange)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)


(defvar treemacs-nerd-icons-tab (propertize "\t" :face 'treemacs-nerd-icons-file-face))
(defvar chevron-down (format "%s%s" (nerd-icons-octicon "nf-oct-chevron_down"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab))
(defvar chevron-right (format "%s%s" (nerd-icons-octicon "nf-oct-chevron_right"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab))

(treemacs-create-theme "custom-nerd-icons"
  :config
  (progn
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (cadr (cdr item))) '(:v-adjust -0.05 :height 1.0) (cdr (cddr item))))
             (icon (apply func args)))
        (let* ((icon-pair (cons (format " %s%s%s" treemacs-nerd-icons-tab icon treemacs-nerd-icons-tab) (format " %s%s%s" treemacs-nerd-icons-tab icon treemacs-nerd-icons-tab)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon  (car icon-pair))
               (tui-icon  (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))

    ;; Custom root
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-repo"  :face 'treemacs-nerd-icons-root-face) treemacs-nerd-icons-tab)
                          :extensions (root-closed root-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (dir-closed)
                          :fallback 'same-as-icon)
    ;; Custom directory
    ;; src
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-blue-alt) treemacs-nerd-icons-tab)
                          :extensions ("src-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-blue-alt) treemacs-nerd-icons-tab)
                          :extensions ("src-closed")
                          :fallback 'same-as-icon)
    ;; main
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("main-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("main-closed")
                          :fallback 'same-as-icon)
    ;; test
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-lgreen) treemacs-nerd-icons-tab)
                          :extensions ("test-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-lgreen) treemacs-nerd-icons-tab)
                          :extensions ("test-closed")
                          :fallback 'same-as-icon)
    ;; bin
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_zip" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("bin-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_zip" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("bin-closed")
                          :fallback 'same-as-icon)
    ;; public
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_account" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("public-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_account" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("public-closed")
                          :fallback 'same-as-icon)
    ;; private
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_cancel" :face 'nerd-icons-dred) treemacs-nerd-icons-tab)
                          :extensions ("private-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_cancel" :face 'nerd-icons-dred) treemacs-nerd-icons-tab)
                          :extensions ("private-closed")
                          :fallback 'same-as-icon)
    ;; build
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_wrench_outline" :face 'nerd-icons-lorange) treemacs-nerd-icons-tab)
                          :extensions ("build-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_wrench_outline" :face 'nerd-icons-lorange) treemacs-nerd-icons-tab)
                          :extensions ("build-closed")
                          :fallback 'same-as-icon)
    ;; lib
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-dpurple) treemacs-nerd-icons-tab)
                          :extensions ("lib-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-dpurple) treemacs-nerd-icons-tab)
                          :extensions ("lib-closed")
                          :fallback 'same-as-icon)
    ;; var
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-pink) treemacs-nerd-icons-tab)
                          :extensions ("var-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-pink) treemacs-nerd-icons-tab)
                          :extensions ("var-closed")
                          :fallback 'same-as-icon)
    ;; temp
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_clock" :face 'nerd-icons-lorange) treemacs-nerd-icons-tab)
                          :extensions ("temp-open" "tmp-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_clock" :face 'nerd-icons-lorange) treemacs-nerd-icons-tab)
                          :extensions ("temp-closed" "tmp-closed")
                          :fallback 'same-as-icon)
    ;; config
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_cog" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("config-open" "configuration-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_cog" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("config-closed" "configuration-closed")
                          :fallback 'same-as-icon)
    ;; settings
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_wrench" :face 'nerd-icons-cyan-alt) treemacs-nerd-icons-tab)
                          :extensions ("settings-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_wrench" :face 'nerd-icons-cyan-alt) treemacs-nerd-icons-tab)
                          :extensions ("settings-closed")
                          :fallback 'same-as-icon)
    ;; tools
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_wrench_outline" :face 'nerd-icons-cyan-alt) treemacs-nerd-icons-tab)
                          :extensions ("tools-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_wrench_outline" :face 'nerd-icons-cyan-alt) treemacs-nerd-icons-tab)
                          :extensions ("tools-closed")
                          :fallback 'same-as-icon)
    ;; java
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-lmaroon) treemacs-nerd-icons-tab)
                          :extensions ("java-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-lmaroon) treemacs-nerd-icons-tab)
                          :extensions ("java-closed")
                          :fallback 'same-as-icon)
    ;; resources
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_text" :face 'nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("resources-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_text" :face 'nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("resources-closed")
                          :fallback 'same-as-icon)
    ;; application
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_text_outline" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("application-open" "app-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_text_outline" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("application-closed" "app-closed")
                          :fallback 'same-as-icon)
    ;; domain
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_home" :face 'nerd-icons-dcyan) treemacs-nerd-icons-tab)
                          :extensions ("domain-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_home" :face 'nerd-icons-dcyan) treemacs-nerd-icons-tab)
                          :extensions ("domain-closed")
                          :fallback 'same-as-icon)
    ;; model
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_eye_outline" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("model-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_eye_outline" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("model-closed")
                          :fallback 'same-as-icon)
    ;; controller
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_cog_outline" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("controllers-open" "controller-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_cog_outline" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("controllers-closed" "controller-closed")
                          :fallback 'same-as-icon)
    ;; services
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_text_outline" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("services-open" "service-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_text_outline" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("services-closed" "service-closed")
                          :fallback 'same-as-icon)
    ;; repository
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_table" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("repository-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_table" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("repository-closed")
                          :fallback 'same-as-icon)
    ;; modules
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_multiple_outline" :face 'nerd-icons-lmaroon) treemacs-nerd-icons-tab)
                          :extensions ("module-open" "modules-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_multiple_outline" :face 'nerd-icons-lmaroon) treemacs-nerd-icons-tab)
                          :extensions ("module-closed" "modules-closed")
                          :fallback 'same-as-icon)
    ;; security
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_lock" :face 'nerd-icons-lyellow) treemacs-nerd-icons-tab)
                          :extensions ("security-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_lock" :face 'nerd-icons-lyellow) treemacs-nerd-icons-tab)
                          :extensions ("security-closed")
                          :fallback 'same-as-icon)
    ;; templates
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_file_outline" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("template-open" "templates-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_file_outline" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("template-closed" "templates-closed")
                          :fallback 'same-as-icon)
    ;; plugins
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_multiple_plus" :face 'nerd-icons-pink) treemacs-nerd-icons-tab)
                          :extensions ("plugins-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_multiple_plus" :face 'nerd-icons-pink) treemacs-nerd-icons-tab)
                          :extensions ("plugins-closed")
                          :fallback 'same-as-icon)
    ;; utils
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_plus" :face 'nerd-icons-lpink) treemacs-nerd-icons-tab)
                          :extensions ("utils-open" "util-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_plus" :face 'nerd-icons-lpink) treemacs-nerd-icons-tab)
                          :extensions ("utils-closed" "util-closed")
                          :fallback 'same-as-icon)
    ;; error
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_information" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("security-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_information" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("security-closed")
                          :fallback 'same-as-icon)
    ;; custom
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_edit" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("custom-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_edit" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("custom-closed")
                          :fallback 'same-as-icon)
    ;; deploy
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_arrow_up" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("deploy-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_arrow_up" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("deploy-closed")
                          :fallback 'same-as-icon)
    ;; api
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_upload" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("api-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_upload" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("api-closed")
                          :fallback 'same-as-icon)
    ;; rest
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_pound_outline" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("rest-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_pound_outline" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions ("rest-closed")
                          :fallback 'same-as-icon)
    ;; client
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_account_outline" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("client-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_account_outline" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("client-closed")
                          :fallback 'same-as-icon)
    ;; jobs
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_play" :face ''nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("job-open" "jobs-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_play" :face ''nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("job-closed" "jobs-closed")
                          :fallback 'same-as-icon)
    ;; data
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_file_outline" :face 'nerd-icons-lsilver) treemacs-nerd-icons-tab)
                          :extensions ("data-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_file_outline" :face 'nerd-icons-lsilver) treemacs-nerd-icons-tab)
                          :extensions ("data-closed")
                          :fallback 'same-as-icon)
    ;; db, sql
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_table_outline" :face 'nerd-icons-dgreen) treemacs-nerd-icons-tab)
                          :extensions ("db-open" "sql-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_table_outline" :face 'nerd-icons-dgreen) treemacs-nerd-icons-tab)
                          :extensions ("db-closed" "sql-closed")
                          :fallback 'same-as-icon)
    ;; sql
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_table" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("sql-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_table" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("sql-closed")
                          :fallback 'same-as-icon)
    ;; doc
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_file" :face 'nerd-icons-dblue) treemacs-nerd-icons-tab)
                          :extensions ("doc-open" "docs-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_file" :face 'nerd-icons-dblue) treemacs-nerd-icons-tab)
                          :extensions ("doc-closed""docs-closed")
                          :fallback 'same-as-icon)
    ;; key
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_key" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("key-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_key" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("key-closed")
                          :fallback 'same-as-icon)
    ;; github
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-sucicon "nf-custom-folder_github" :face 'nerd-icons-lsilver) treemacs-nerd-icons-tab)
                          :extensions ("github-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-sucicon "nf-custom-folder_github" :face 'nerd-icons-lsilver) treemacs-nerd-icons-tab)
                          :extensions ("github-closed")
                          :fallback 'same-as-icon)
    ;; git
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-sucicon "nf-custom-folder_git" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("git-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-sucicon "nf-custom-folder_git" :face 'nerd-icons-orange) treemacs-nerd-icons-tab)
                          :extensions ("git-closed")
                          :fallback 'same-as-icon)
    ;; icons, img, image
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_image" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("icons-open" "img-open" "image-open" "images-open" "screenshots-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_image" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("icons-closed" "img-closed" "image-closed" "images-closed" "screenshots-open")
                          :fallback 'same-as-icon)
    ;; docker
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("docker-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("docker-closed")
                          :fallback 'same-as-icon)
    ;; npm
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-sucicon "nf-custom-folder_npm" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("npm-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-sucicon "nf-custom-folder_npm" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("npm-closed")
                          :fallback 'same-as-icon)
    ;; lisp
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-faicon "nf-fa-folder_open" :face 'nerd-icons-purple) treemacs-nerd-icons-tab)
                          :extensions ("lisp-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-faicon "nf-fa-folder" :face 'nerd-icons-purple) treemacs-nerd-icons-tab)
                          :extensions ("lisp-closed")
                          :fallback 'same-as-icon)
    ;; ci
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-mdicon "nf-md-folder_arrow_right" :face 'nerd-icons-lblue) treemacs-nerd-icons-tab)
                          :extensions ("ci-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_arrow_right" :face 'nerd-icons-lblue) treemacs-nerd-icons-tab)
                          :extensions ("ci-closed")
                          :fallback 'same-as-icon)
    ;; download
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_download" :face 'nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("download-open" "downloads-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_download" :face 'nerd-icons-maroon) treemacs-nerd-icons-tab)
                          :extensions ("download-closed" "downloads-closed")
                          :fallback 'same-as-icon)
    ;; pictures
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_image" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("pictures-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_image" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("pictures-closed")
                          :fallback 'same-as-icon)
    ;; documents
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_file" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("documents-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_file" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("documents-closed")
                          :fallback 'same-as-icon)
    ;; music
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_music" :face 'nerd-icons-purple) treemacs-nerd-icons-tab)
                          :extensions ("music-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_music" :face 'nerd-icons-purple) treemacs-nerd-icons-tab)
                          :extensions ("music-closed")
                          :fallback 'same-as-icon)
    ;; videos
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down(nerd-icons-mdicon "nf-md-folder_play" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("videos-open")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-mdicon "nf-md-folder_play" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions ("videos-closed")
                          :fallback 'same-as-icon)
    ;; Custom icons
    ;; binary
    (treemacs-create-icon :icon (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-codicon "nf-cod-file_binary" :face 'nerd-icons-lmaroon) treemacs-nerd-icons-tab)
                          :extensions ("exe" "dll" "obj" "so" "o")
                          :fallback 'same-as-icon)
    ;; shell, bash
    (treemacs-create-icon :icon (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-devicon "nf-dev-terminal" :face 'nerd-icons-purple) treemacs-nerd-icons-tab)
                          :extensions ("sh" "profile")
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-devicon "nf-dev-terminal" :face 'nerd-icons-dpink) treemacs-nerd-icons-tab)
                          :extensions ("bashrc" "bash" "bash_profile" "bashrc" "bash_login" "bash_aliases")
                          :fallback 'same-as-icon)
    ;; zsh
    (treemacs-create-icon :icon (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-devicon "nf-dev-terminal" :face 'nerd-icons-lcyan) treemacs-nerd-icons-tab)
                          :extensions ("zsh" "zshrc" "zshenv" "zprofile" "zlogin" "zlogout")
                          :fallback 'same-as-icon)
    ;; config
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-sucicon "nf-seti-config" :face 'nerd-icons-lorange) treemacs-nerd-icons-tab)
                          :extensions ("conf" "config" "cfg" "xdefaults" "xresources" "terminalrc" "ledgerrc")
                          :fallback 'same-as-icon)
    ;; license
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-octicon "nf-oct-book" :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions ("LICENSE")
                          :fallback 'same-as-icon)
    ;; readme
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-octicon "nf-oct-book" :face 'nerd-icons-lcyan) treemacs-nerd-icons-tab)
                          :extensions ("readme")
                          :fallback 'same-as-icon)
    ;; Makefile
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-sucicon "nf-seti-makefile" :face 'nerd-icons-dorange) treemacs-nerd-icons-tab)
                          :extensions ("Makefile")
                          :fallback 'same-as-icon)
    ;; diff
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-octicon "nf-oct-diff" :face 'nerd-icons-lred) treemacs-nerd-icons-tab)
                          :extensions ("diff")
                          :fallback 'same-as-icon)
    ;; db
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-sucicon "nf-seti-db" :face 'nerd-icons-cyan) treemacs-nerd-icons-tab)
                          :extensions ("db")
                          :fallback 'same-as-icon)
    ;; bookmarks
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-octicon "nf-oct-bookmark" :face 'nerd-icons-lpink) treemacs-nerd-icons-tab)
                          :extensions ("bookmarks")
                          :fallback 'same-as-icon)
    ;; vimrc tridactylrc vimperatorrc ideavimrc vrapperrc
    (treemacs-create-icon :icon  (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-sucicon "nf-custom-vim" :face 'nerd-icons-green) treemacs-nerd-icons-tab)
                          :extensions ("vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc")
                          :fallback 'same-as-icon)

    ;; Others
    (treemacs-create-icon :icon (format "%s%s%s" chevron-down (nerd-icons-octicon "nf-oct-package" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (tag-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s" chevron-right (nerd-icons-octicon "nf-oct-package" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (tag-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-tag"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (tag-leaf)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-flame" :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                          :extensions (error)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-stop" :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                          :extensions (warning)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-info"  :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                          :extensions (info)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-mail"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (mail)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-bookmark"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (bookmark)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-monitor"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (screen)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-home"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (house)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-list"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (list)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-repeat"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (repeat)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-suitcase"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (suitcase)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-close"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (close)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-calendar"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (calendar)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-briefcase"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (briefcase)
                          :fallback 'same-as-icon)
    ;; default icon
    (treemacs-create-icon :icon (format " %s%s%s" treemacs-nerd-icons-tab (nerd-icons-faicon "nf-fa-file_o" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                          :extensions (fallback)
                          :fallback 'same-as-icon)))


(provide 'my-custom-treemacs-theme)
;;; my-custom-treemacs-theme.el ends here
