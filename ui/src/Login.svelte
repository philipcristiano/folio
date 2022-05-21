<script>
    export let socket;
    import { createEventDispatcher } from 'svelte';

    let username = "";
    let password = "";

	function login() {
        socket.send(JSON.stringify({what:"login",
                                    "username": username,
                                    "password": password}
        ));
	}
</script>

<div>
<form on:submit|preventDefault={login}>
  <div class="container">
    <label for="uname"><b>Username</b></label>
    <input type="text" placeholder="Enter Username" bind:value={username} required>

    <label for="psw"><b>Password</b></label>
    <input type="password" placeholder="Enter Password" bind:value={password} name="psw" required>

    <button>Login</button>
  </div>

</form>
</div>
