<script>
	import { onMount } from 'svelte';
	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';


	onMount(() => {
	});

</script>

<main class="flex container xl">
    <div class="columns-2">
        <IntegrationList />
        <TransactionList />
    </div>

</main>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
