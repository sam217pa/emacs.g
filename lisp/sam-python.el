
(use-package python
  :mode (("\\.py\\'" . python-mode))
  :custom
  ;; see https://github.com/jorgenschaefer/elpy/issues/1550 on this.
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")
  (python-indent-offset 4))

(use-package snakemake-mode
  :mode (("Snakefile" . snakemake-mode)
         ("\\.smk\\'" . snakemake-mode)))

(use-package snakemake
  :after snakemake-mode)

(use-package elpy
  :commands (elpy-mode)
  :config
  (setq elpy-rpc-python-command
        (expand-file-name "~/.virtualenvs/keras/bin/python3.7"))
  (setq elpy-shell-echo-input nil)
  (setq elpy-shell-echo-input-cont-prompt nil)
  (setq elpy-shell-echo-input-lines-head nil))

(provide 'sam-python)
