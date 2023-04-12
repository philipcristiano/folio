<script lang="ts">

  import { onMount } from 'svelte';
  import Time from "svelte-time";
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

    let path = "/transactions?" + new URLSearchParams(params);
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

<div class="table bg-white text-left text-sm overflow-x-auto">

  <div class="table-header-group bg-gray-50">
      <div class="table-row">
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Datetime</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Provider</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Symbol</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Direction</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Amount</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Description</div>
        <div class="table-cell px-6 py-2 font-medium text-gray-900">Account ID</div>
      </div>
  </div>
<div class="table-row-group">
{#each transactions as tx}
<div class="table-row hover:bg-gray-50" >
    <div class="table-cell"><Time relative timestamp="{tx.timestamp}"/></div>
    <div class="table-cell">{tx.provider_name}</div>
    <div class="table-cell">{tx.symbol}</div>
    <div class="table-cell">{tx.direction}</div>
    <div class="table-cell">{tx.amount}</div>
    <div class="table-cell">{tx.description}</div>
    <div class="table-cell">{tx.external_id}</div>
</div>
{/each}
</div>

</div>
</div>
