# rain

☔

Minimal IRC client in elm.

![screenshot](http://i.imgur.com/PwdHbsl.png)

build dependencies:

  - [elm](https://guide.elm-lang.org/install.html)
  - [yarn](https://yarnpkg.com/en/docs/install) - dependencies for `wsproxy`. otherwise just use `npm install`
  - [entr](https://github.com/clibs/entr) - for `make watch`
  
```  
    # Install dependencies, build, and run:
    make all dev
    
    # WebSocket proxy:
    ws://localhost:6676
    
    # Frontend:
    http://localhost:8000
```
