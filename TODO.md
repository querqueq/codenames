# codenames

## Plan

* Implement a REST web service with servant for making moves
* Expose SSE stream for changes so client can update
* Write client with a simple frontend library reacting to SSE stream
* Create list of words via [SPARQL](https://hackage.haskell.org/package/hsparql)

## Next Steps

1.  ~~Write whole Lobby and Game logic~~
2.  ~~Make getting/modifying of state generalized~~
3.  ~~Add uuids for id instead of fixed id 12~~ or use sparql to get short readable names for ids
4.  Authorize player actions
5.  Return error with content
6.  Map errors to appropriate http status codes
7.  Logging!
8.  Implement a minimal Lobby client
9.  ???
10. Add sparql for game creation
11. Host on heroku
12. ???
