<script lang="ts">

  import { onMount } from 'svelte';
  import Button from './Button.svelte';

  import Integration from './Integration.svelte';
  import IntegrationSetup from './IntegrationSetup.svelte';

  export let transaction_filters = {};

  let message = "";
  let address = "";
  let integrations = [];


  async function getIntegrations() {
    let response = await fetch("/api/integrations", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integrations = json.integrations;
    } else {
        message = json.message;
    };
  }
  async function syncIntegration(integration) {
    integration['accounts'] = [];
    let response = await fetch("/api/integrations/" + integration.id + "/sync", {
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
    if (integration.delete_enabled ) {
      let response = await fetch("/api/integrations/" + integration.id, {
          method: "DELETE",
      });
      let json = await response.json()
      if (response.ok) {
          message = "Integration Deleted";
          getIntegrations();
      } else {
          message = json.message;
      }
    } else {
        integration.delete_enabled = true;
        integrations = integrations;
    }
  }
  async function filterForIntegration(integration) {
      transaction_filters = {"integration_id": integration.id};
  }

  onMount(() => {
      getIntegrations();
  });

</script>

{#if message}
    <div><h2> {message} </h2></div>
{/if}

<div class="border-grey max-w-sm shadow-lg border-1 p-3 columns-1 columns-3xs">

{#each integrations as integration (integration.id)}
<div class="border-grey max-w-sm shadow-lg border-1 p-1">
    <Integration {...integration} />
    <Button on:click={() => syncIntegration(integration)}>Sync</Button>
    <Button on:click={() => filterForIntegration(integration)}>Transactions</Button>
    {#if integration.delete_enabled }
      Are you sure?
    {/if}
    <Button on:click={() => deleteIntegration(integration)}>Delete</Button>
</div>
{/each}

<IntegrationSetup />

</div>
