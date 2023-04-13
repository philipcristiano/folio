<script>


	import { onMount } from 'svelte';

  import { Router, Link, Route } from "svelte-routing";

  import BoxContent from './BoxContent.svelte';
  import BoxSidebar from './BoxSidebar.svelte';

	import IntegrationList from './IntegrationList.svelte';
	import TransactionList from './TransactionList.svelte';

  export let url = "";

  let transaction_filters = {};

	onMount(() => {
	});

</script>
<Router url="{url}">
    <div class="sticky top-0 p-4 w-full">
      <Link to="/">Folio</Link>
      <Link to="/about">About</Link>
      <Link to="/blog">Blog</Link>
    </div>
<body><div class="container mx-auto">

  <div class="flex flex-row flex-wrap py-1">
    <BoxSidebar>
      <IntegrationList bind:transaction_filters={transaction_filters} />
    </BoxSidebar>

    <BoxContent>

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
