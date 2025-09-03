;; -*- lexical-binding: t; nameless-current-name: "mood"; -*-

(unless (feature! -grep-a-lot)
  (use-package grep-a-lot
    :config
    (when (plist-get (feature! :grep-a-lot) :keys)
      (grep-a-lot-setup-keys)))

  (use-package grep-a-lot
    :after vc-git
    :config
    (grep-a-lot-advise vc-git-grep)))
