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
	});

</script>

<main class="flex container xl ">
    <h1>Hello!</h1>
    <AccountList />

</main>

<style>
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
