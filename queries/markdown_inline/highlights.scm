; extends

; Tags
(tag) @markup.tag

; Set url for wiki links
((wiki_link
  (link_destination) @_url) @_label
  (#set! @_label url @_url))

; Conceal brackets and pipe of wiki links
(wiki_link
  [
    "["
    "]"
    "|"
  ] @markup.link
  (#set! conceal ""))

; Conceal destination of wiki links with display text
(wiki_link
  (link_destination) @conceal (#set! conceal "")
  (link_text))
