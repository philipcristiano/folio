<script>
    export let socket;

	import { onMount } from 'svelte';
	import Login from './Login.svelte';

    export let items = [];
    let list_fetched = false;
    let user = false;
    let coinbase_user = false;
    let coinbase_accounts = [];
    let coinbase_transactions = [];
    let user_token;

    function addItem() {
		items = [...items, {temp_id: items.length + 1, content: "Fill me in!"}];
        socket.send(JSON.stringify({what:"new item"}));
        socket.send(JSON.stringify({what:"list", list: items}));
	}
    function start() {
        socket.send(JSON.stringify({what:"start"}));
	}
    function removeItem(id) {
        items = items.filter(i => i.id !== id);
        socket.send(JSON.stringify({what:"remove item", id: id}));
    }
    function refreshList() {
        socket.send(JSON.stringify({what: "get list"}));
        list_fetched = true;
    }
	onMount(() => {
        console.log(socket.readyState);
        socket.onopen = function(e) {
            user_token = sessionStorage.getItem('user_token');
            if (user_token) {
                socket.send(JSON.stringify({what:"token login", token: user_token}));
            }
        };
	});
    function handleIdAssignedMessage(event) {
        console.log("handleIdAssignedMessage");
        function setTempId(i) {
            if(i.temp_id == event.temp_id) {
                console.log("Setting ID");
                i.id = event.id;
                delete i.temp_id;
            }
            return i
        }
        items = items.map(setTempId);

    }
    function handleLoggedInMessage(event) {
        start();
        user = event.username;
        user_token = event.token;
        sessionStorage.setItem('user_token', user_token);
	};
    function handleCoinbaseUser(event) {
        coinbase_user = event.user;
	}
    function handleCoinbaseAccounts(event) {
        coinbase_accounts = [...coinbase_accounts, ...event.accounts];
	}
    function handleCoinbaseTransactions(event) {
        coinbase_transactions = [...coinbase_transactions, ...event.transactions];
	}

    socket.onmessage = function(event) {
        var msg = JSON.parse(event.data);
        console.log(msg);

        switch(msg.what) {
        case "list":
            items = msg.list;
            break;
        case "id_assigned":
            handleIdAssignedMessage(msg);
            break;
        case "logged in":
            handleLoggedInMessage(msg);
            break;
        case "coinbase_user":
            handleCoinbaseUser(msg)
            break;
        case "coinbase_accounts":
            handleCoinbaseAccounts(msg)
            break;
        case "coinbase_transactions":
            handleCoinbaseTransactions(msg)
            break;
        default:
            alert(msg.what);
        }

    }

</script>

<main>
{#if user }
	    <h1>Hello!</h1>
    {#each items as item (item.id)}
    <div>
        <button on:click={removeItem(item.id)}> Remove item</button>
    </div>
    {/each}

    {#if coinbase_user }
        { coinbase_user.name }
    {/if}
    {#each coinbase_accounts as coinbase_account (coinbase_account.id)}
    <div>
        { coinbase_account.symbol }
        { coinbase_account.balance }
    </div>
    {/each}
    <div>
    {#each coinbase_transactions as coinbase_transaction (coinbase_transaction.id)}
        <div>
            { coinbase_transaction }
            { coinbase_transaction.details.header }
            { coinbase_transaction.details.subtitle }
        </div>
    {/each}
    </div>
{:else}
    <Login bind:socket />
{/if}

</main>

<style>
	main {
		text-align: center;
		padding: 1em;
		max-width: 240px;
		margin: 0 auto;
	}

	h1 {
		color: #ff3e00;
		text-transform: uppercase;
		font-size: 4em;
		font-weight: 100;
	}

	@media (min-width: 640px) {
		main {
			max-width: none;
		}
	}
</style>
