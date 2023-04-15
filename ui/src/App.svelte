<script>


	import { onMount } from 'svelte';

  import { Router, Link, Route } from "svelte-routing";

  import BoxContent from './BoxContent.svelte';
  import BoxSidebar from './BoxSidebar.svelte';

	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';
	import AssetList from './AssetList.svelte';

  export let url = "";

  let transaction_filters = {};

	onMount(() => {
	});

</script>
<Router url="{url}">
<body><div class="container mx-auto">
    <div class="sticky top-0 p-4 w-full">
      <Link to="/">Folio</Link>
      <Link to="/assets">Assets</Link>
    </div>

  <div class="flex flex-row flex-wrap py-1 bg:white">
    <BoxSidebar>
      <IntegrationList bind:transaction_filters={transaction_filters} />
    </BoxSidebar>

    <BoxContent>

      <Route path="/assets">
        <AssetList />
      </Route>

      <Route path="/">
        <TransactionList bind:transaction_filters={transaction_filters} />
      </Route>
    </BoxContent>
  </div>

</div></body>
</Router>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
