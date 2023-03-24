<script lang="ts">

  import { onMount } from 'svelte';

  import Integration from './Integration.svelte';
  import IntegrationSetup from './IntegrationSetup.svelte';

  let message = "";
  let address = "";
  let integrations = [];
  let transactions = [];

  async function getIntegrations() {
    let response = await fetch("/integrations", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        json.integrations.forEach(i =>
            getIntegrationAccounts(i));
        integrations = json.integrations;
    } else {
        message = json.message;
    };
  }
  async function syncIntegration(integration) {
    integration['accounts'] = [];
    let response = await fetch("/integrations/" + integration.id + "/sync", {
        method: "POST",
    });
    let json = await response.json()
    if (response.ok) {
        message = "Starting integration sync" + integration.id;
    } else {
        message = json.message;
    };
  }
  async function deleteIntegration(integration) {
    let response = await fetch("/integrations/" + integration.id, {
        method: "DELETE",
    });
    let json = await response.json()
    if (response.ok) {
        message = "Integration Deleted";
        getIntegrations();
    } else {
        message = json.message;
    };
  }
  async function getIntegrationAccounts(integration) {
    integration['accounts'] = [];
    let response = await fetch("/integrations/" + integration.id + "/accounts", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integration['accounts'] = json.accounts;
        integrations = integrations;
    } else {
        message = json.message;
    };
  }
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
      getIntegrations();
      getTransactions();
  });

</script>

<div class="columns-6">
    <div class="max-w-md">
        {#if message}
            <div><h2> {message} </h2></div>
        {/if}

        <IntegrationSetup />

        Current installed integrations:
        {#each integrations as integration (integration.id)}
        <div>
            <Integration {...integration} />
            <button type="submit" on:click={() => syncIntegration(integration)}>Sync</button>
            <button type="submit" on:click={() => deleteIntegration(integration)}>Delete</button>
        </div>
        {/each}


    </div>

    <div class="max-w-md">
        Transactions:
        <table class="w-full border-collapse bg-white text-left text-sm text-gray-500">
        <thead class="bg-gray-50">
            <tr>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Datetime</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Provider</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Symbol</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Direction</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Amount</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Description</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Account ID</th>
            </tr>
        </thead>
        <tbody>
        {#each transactions as tx}
        <tr>
            <td>{tx.timestamp}</td>
            <td>{tx.provider_name}</td>
            <td>{tx.symbol}</td>
            <td>{tx.direction}</td>
            <td>{tx.amount}</td>
            <td>{tx.description}</td>
            <td>{tx.external_id}</td>
        </tr>
        {/each}

        </table>

    </div>

</div>
