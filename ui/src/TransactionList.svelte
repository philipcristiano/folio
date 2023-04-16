<script lang="ts">

  import { onMount } from 'svelte';
  import Time from "svelte-time";
  import Table from './Table.svelte';
  import TableElementTime from './TableElementTime.svelte';
  import FilterButton from './FilterButton.svelte';


  let message = "";
  export let transaction_filters = {};

  let transactions = [];
  $: getTransactions(transaction_filters);

  function transaction_filters_to_params(filters) {
      return filters
  };
  function clearFilter(key) {
    delete transaction_filters[key];
    transaction_filters = transaction_filters;
  }

  async function getTransactions(filters) {
    let params = transaction_filters_to_params(filters);

    let path = "/api/transactions?" + new URLSearchParams(params);
    let response = await fetch(path, {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        transactions = json.transactions;
    } else {
        message = json.message;
    };
  }

  onMount(() => {
      getTransactions();
  });

  let fields = [
    {name: "timestamp", title: "Datetime", component: TableElementTime},
    {name: "provider_name", title: "Provider"},
    {name: "symbol", title: "Symbol"},
    {name: "direction", title: "Direction"},
    {name: "amount", title: "Amount"},
    {name: "description", title: "Description"},
    {name: "external_id", title: "Account ID"},
  ];

</script>

{#if message}
    <div><h2> {message} </h2></div>
{/if}


<div class="w-full">
  <div class="">

  Filters:

  {#if transaction_filters.integration_id}
    <FilterButton on:click={() => clearFilter("integration_id")}>Integration:
    { transaction_filters.integration_id }
  </FilterButton>

  {/if}

  </div>

<Table fields={fields} data={transactions} />

</div>
