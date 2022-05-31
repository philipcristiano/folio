import App from './App.svelte';

var loc = window.location, new_uri;
if (loc.protocol === "https:") {
    new_uri = "wss:";
} else {
    new_uri = "ws:";
}
new_uri += "//" + loc.host;
new_uri += loc.pathname + "ws";
var ws = new WebSocket(new_uri);


const app = new App({
	target: document.body,
	props: {
    socket: ws
	}
});

export default app;
