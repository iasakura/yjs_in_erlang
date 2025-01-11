# yjs_in_erlang

This is a pure Erlang implementation of Yjs/Yrs, created as a learning project. It includes a simple demo to showcase basic functionality.
**Note:** This library is not intended for production use.

## Demo

### Backend

Start the backend with the following command:

```sh
rebar3 shell
```

### Frontend

Set up and run the frontend demo:

```sh
cd quill-demo
npm install
npm start
```

Once everything is running, you can view the demo at [http://localhost:8080/quill](http://localhost:8080/quill).

## Supervisor tree

![](./doc/supervisor.drawio.svg)

---

## Credits

The `quill-demo` directory contains files sourced from [yjs-demos/quill](https://github.com/yjs/yjs-demos/tree/main/quill).
