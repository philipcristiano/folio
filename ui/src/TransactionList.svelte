<script lang="ts">

  import { onMount } from 'svelte';
  import Time from "svelte-time";

  let message = "";
  let transactions = [];

  async function getTransactions(integration) {
    let response = await fetch("/transactions", {
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

<div class="table border-collapse bg-white text-left text-sm overflow-x-visible table">

  <div class="table-header-group bg-gray-50">
      <div class="table-row">
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Datetime</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Provider</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Symbol</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Direction</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Amount</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Description</div>
        <div scope="col" class="table-cell px-6 py-2 font-medium text-gray-900">Account ID</div>
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
