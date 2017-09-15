# codenames

## Plan

* Implement a REST web service with servant for making moves
* Expose SSE stream for changes so client can update
* Write client with a simple frontend library reacting to SSE stream
* Create list of words via [SPARQL](https://hackage.haskell.org/package/hsparql)

## Next Steps

1.  ~~Write whole Lobby and Game logic~~
2.  ~~Make the getting/modifying of state generalized~~
3.  Authorize player actions
4.  Returns error with content
5.  Map errors to appropriate http status codes
6.  Logging!
7.  Implement a minimal Lobby client
8.  ???
9.  Add sparql for game creation
