(load "readwise.el")

(describe "readwise.el"
  :var (hl)
  (before-each (setq hl (json-read-file "test.json")))

  (it "extracts next page cursor"
    (expect (readwise--get-cursor hl) :to-equal "foo"))

  (it "and so is a spec"
    (expect (readwise--get-cursor hl) :to-equal "foo")))
