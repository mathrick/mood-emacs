(defflag :style 'emacs "Completion style to use. Valid choices
are 'orderless, 'prescient, 'emacs (meaning use `completion-styles')
or nil (meaning use selectrum's default)")

(defflag -extra-separators "Do not configure slashes and hyphens
in addition to spaces as component separators. Only meaningful in
completion styles that support custom separators (currently only
orderless)")

(defflag -history "Don't store and use completion history to
boost frequent/recent candidates. With 'prescient, this only
disables persistent storage of history, as usage-based sorting is
built in")
