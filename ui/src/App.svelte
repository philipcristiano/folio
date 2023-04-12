<script>
	import { onMount } from 'svelte';
  import BoxContent from './BoxContent.svelte';
  import BoxSidebar from './BoxSidebar.svelte';

	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';

  let transaction_filters = {};

	onMount(() => {
	});

</script>

<main class="columns-1 md:columns-6 ">
  <BoxSidebar><IntegrationList bind:transaction_filters={transaction_filters} /></BoxSidebar>
  <BoxContent><TransactionList bind:transaction_filters={transaction_filters} /></BoxContent>

</main>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
