<script lang="ts">

  import { onMount } from 'svelte';

  import Integration from './Integration.svelte';
  import IntegrationSetup from './IntegrationSetup.svelte';

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

  onMount(() => {
      getBalance();
      getIntegrations();
  });

</script>

<div class="max-w-sm border-black border-indigo-900 shadow-lg">
    {#if message}
        <div><h2> {message} </h2></div>
    {/if}

    Balance: ${ balance }

    <IntegrationSetup />

    Current installed integrations:
    {#each integrations as integration (integration.id)}
    <div class="max-w-md">
        <Integration {...integration} />
        <button type="submit" on:click={() => syncIntegration(integration)}>Sync</button>
        <button type="submit" on:click={() => deleteIntegration(integration)}>Delete</button>
    </div>
    <hr />
    {/each}


</div>
