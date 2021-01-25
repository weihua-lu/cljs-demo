# cljs-demo

Played a little bit with cljs with reagent and datascript with this demo SPA

https://weihua-lu.github.io/cljs-demo/

### run (with cider-nrepl)
1. `npm install`
2. `clj -M:dev watch main` (needs latest clojure 1.10.1.697 above)

### release
`clj -M:dev relaese main`

### user story
- User search term in wiki and find related links(inbound) and linkshere(outbound)
- Search hint possible terms from previous related results
- Find possible relationship between different terms by searching their links and linkshere
