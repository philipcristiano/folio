<script>
	import { onMount } from 'svelte';
  import BoxContent from './BoxContent.svelte';
  import BoxSidebar from './BoxSidebar.svelte';

	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';

	onMount(() => {
	});

</script>

<main class="flex container px-4 columns-2">
  <BoxSidebar><IntegrationList /></BoxSidebar>
  <BoxContent><TransactionList /></BoxContent>

</main>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
