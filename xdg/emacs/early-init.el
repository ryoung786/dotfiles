;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-message t)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq default-frame-alist '((fullscreen . maximized)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#263238")
			    (foreground-color . "white")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
			    ))

;; test
(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/14:/opt/homebrew/lib/gcc/14/gcc/aarch64-apple-darwin24/14")
