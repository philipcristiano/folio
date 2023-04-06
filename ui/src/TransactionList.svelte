<script lang="ts">

  import { onMount } from 'svelte';
  import Time from "svelte-time";

  import BoxContent from './BoxContent.svelte';
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

<BoxContent>
        {#if message}
            <div><h2> {message} </h2></div>
        {/if}

        <table class="border-collapse bg-white text-left text-sm">
        <thead class="bg-gray-50">
            <tr>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Datetime</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Provider</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Symbol</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Direction</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Amount</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Description</th>
              <th scope="col" class="px-6 py-2 font-medium text-gray-900">Account ID</th>
            </tr>
        </thead>
        <tbody>
        {#each transactions as tx}
        <tr>
            <td><Time relative timestamp="{tx.timestamp}"/></td>
            <td>{tx.provider_name}</td>
            <td>{tx.symbol}</td>
            <td>{tx.direction}</td>
            <td>{tx.amount}</td>
            <td>{tx.description}</td>
            <td>{tx.external_id}</td>
        </tr>
        {/each}

        </table>


</BoxContent>
