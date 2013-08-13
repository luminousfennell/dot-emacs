;; somehow this is needed, despite of package.el
(require 'evil)
(evil-mode t)

;; disable overriding of evil keymaps
(setq evil-intercept-maps nil
      evil-overriding-maps nil)

;; Remove the bindings for return, space and tab
;; the following code is ripped from the wiki: http://www.emacswiki.org/emacs/Evil#toc7
(defun my-move-key (keymap-from keymap-to key)
     "Moves key binding from one keymap to another, deleting from the old location. "
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil)
     )
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "TAB"))

;; now a long list of modes that should only use the motion keymap
(setq evil-emacs-state-modes nil)
(setq evil-motion-state-modes '(archive-mode
				bbdb-mode
				bookmark-bmenu-mode
				bookmark-edit-annotation-mode
				browse-kill-ring-mode
				bzr-annotate-mode
				calc-mode
				cfw:calendar-mode
				completion-list-mode
				Custom-mode
				custom-theme-choose-mode
				debugger-mode
				delicious-search-mode
				desktop-menu-blist-mode
				desktop-menu-mode
				doc-view-mode
				dvc-bookmarks-mode
				dvc-diff-mode
				dvc-info-buffer-mode
				dvc-log-buffer-mode
				dvc-revlist-mode
				dvc-revlog-mode
				dvc-status-mode
				dvc-tips-mode
				ediff-mode
				ediff-meta-mode
				efs-mode
				Electric-buffer-menu-mode
				emms-browser-mode
				emms-mark-mode
				emms-metaplaylist-mode
				emms-playlist-mode
				etags-select-mode
				fj-mode
				gc-issues-mode
				gdb-breakpoints-mode
				gdb-disassembly-mode
				gdb-frames-mode
				gdb-locals-mode
				gdb-memory-mode
				gdb-registers-mode
				gdb-threads-mode
				gist-list-mode
				gnus-article-mode
				gnus-browse-mode
				gnus-group-mode
				gnus-server-mode
				gnus-summary-mode
				google-maps-static-mode
				ibuffer-mode
				jde-javadoc-checker-report-mode
				magit-commit-mode
				magit-diff-mode
				magit-key-mode
				magit-log-mode
				magit-mode
				magit-reflog-mode
				magit-show-branches-mode
				magit-branch-manager-mode
				magit-stash-mode
				magit-status-mode
				magit-wazzup-mode
				mh-folder-mode
				monky-mode
				notmuch-hello-mode
				notmuch-search-mode
				notmuch-show-mode
				occur-mode
				org-agenda-mode
				package-menu-mode
				proced-mode
				rcirc-mode
				rebase-mode
				recentf-dialog-mode
				reftex-select-bib-mode
				reftex-toc-mode
				sldb-mode
				slime-inspector-mode
				slime-thread-control-mode
				slime-xref-mode
				sr-buttons-mode
				sr-mode
				sr-tree-mode
				sr-virtual-mode
				tar-mode
				tetris-mode
				tla-annotate-mode
				tla-archive-list-mode
				tla-bconfig-mode
				tla-bookmarks-mode
				tla-branch-list-mode
				tla-browse-mode
				tla-category-list-mode
				tla-changelog-mode
				tla-follow-symlinks-mode
				tla-inventory-file-mode
				tla-inventory-mode
				tla-lint-mode
				tla-logs-mode
				tla-revision-list-mode
				tla-revlog-mode
				tla-tree-lint-mode
				tla-version-list-mode
				twittering-mode
				urlview-mode
				vc-annotate-mode
				vc-dir-mode
				vc-git-log-view-mode
				vc-svn-log-view-mode
				vm-mode
				vm-summary-mode
				w3m-mode
				wab-compilation-mode
				xgit-annotate-mode
				xgit-changelog-mode
				xgit-diff-mode
				xgit-revlog-mode
				xhg-annotate-mode
				xhg-log-mode
				xhg-mode
				xhg-mq-mode
				xhg-mq-sub-mode
				xhg-status-extra-mode

				apropos-mode
				Buffer-menu-mode
				calendar-mode
				color-theme-mode
				command-history-mode
				compilation-mode
				dictionary-mode
				ert-results-mode
				help-mode
				Info-mode
				Man-mode
				speedbar-mode
				undo-tree-visualizer-mode
				view-mode woman-mode))
