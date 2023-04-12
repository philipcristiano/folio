<script lang="ts">

  import { onMount } from 'svelte';
  import Button from './Button.svelte';

  import Integration from './Integration.svelte';
  import IntegrationSetup from './IntegrationSetup.svelte';

  export let transaction_filters = {};

  let balance = "";
  let message = "";
  let address = "";
  let integrations = [];

  async function getBalance() {
    let response = await fetch("/balance", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        balance = json.fiat_value;
    } else {
        message = json.message;
    };
  }

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
        integration.state = "running";
        integrations = integrations;
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
  async function filterForIntegration(integration) {
      transaction_filters = {"integration_id": integration.id};
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

  onMount(() => {
      getBalance();
      getIntegrations();
  });

</script>

{#if message}
    <div><h2> {message} </h2></div>
{/if}

<div class="border-grey max-w-sm shadow-lg border-1 p-3">
  <div class="border-grey max-w-sm shadow-lg border-1 p-1">
    <p class="text-lg">Balance: ${ balance }</p>
  </div>
{#each integrations as integration (integration.id)}
<div class="border-grey max-w-sm shadow-lg border-1 p-1">
    <Integration {...integration} />
    <Button on:click={() => syncIntegration(integration)}>Sync</Button>
    <Button on:click={() => deleteIntegration(integration)}>Delete</Button>
    <Button on:click={() => filterForIntegration(integration)}>Transactions</Button>
</div>
{/each}

<IntegrationSetup />
</div>
