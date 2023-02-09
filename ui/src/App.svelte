<script>
    export let socket;

	import { onMount } from 'svelte';
	import Login from './Login.svelte';
	import AccountList from './AccountList.svelte';

    export let items = [];
    let list_fetched = false;
    let user = false;
    let coinbase_user = false;
    let coinbase_accounts = [];
    let coinbase_transactions = [];
    let user_token;

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
        default:
            console.log(msg.what);
        }

    }

</script>

<main>
<h1>Hello!</h1>
    {#if coinbase_user }
        { coinbase_user.name }
    {/if}
    <AccountList />
    <div>
    {#each coinbase_transactions as coinbase_transaction (coinbase_transaction.id)}
        <div>
            { coinbase_transaction }
            { coinbase_transaction.details.header }
            { coinbase_transaction.details.subtitle }
        </div>
    {/each}
    </div>

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
