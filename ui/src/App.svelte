<script>


	import { onMount } from 'svelte';

  import { Router, Link, Route } from "svelte-routing";

  import BoxContent from './BoxContent.svelte';
  import BoxSidebar from './BoxSidebar.svelte';

	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';
	import AssetList from './AssetList.svelte';
  import Balance from './Balance.svelte';

  export let url = "";

  let transaction_filters = {};

	onMount(() => {
	});

</script>
<Router url="{url}">
<body><div class="container mx-auto">
    <div class="sticky top-0 p-4 w-full bg-gray-50">
      <Link to="/">Folio</Link>
      <Link to="/prices">Prices</Link>
      <Balance />
    </div>

  <div class="flex flex-row flex-wrap py-1 bg:white">
    <BoxSidebar>
      <IntegrationList bind:transaction_filters={transaction_filters} />
    </BoxSidebar>

    <BoxContent>

      <Route path="/prices">
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
