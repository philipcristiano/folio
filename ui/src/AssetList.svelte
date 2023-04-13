<script lang="ts">

  import { onMount } from 'svelte';
  import Button from './Button.svelte';


  let message = "";


  async function getAssets() {
    let response = await fetch("/api/assets", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        json.assets.forEach(i =>
            getIntegrationAccounts(i));
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

  onMount(() => {
      getAssets();
  });

</script>

<div class="w-full">
  <div class="">



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
{#each assets as asset}
<div class="table-row hover:bg-gray-50" >
    <div class="table-cell"><Time relative timestamp="{asset.timestamp}"/></div>
    <div class="table-cell">{asset.provider_name}</div>
    <div class="table-cell">{asset.symbol}</div>
    <div class="table-cell">{asset.direction}</div>
    <div class="table-cell">{asset.amount}</div>
    <div class="table-cell">{asset.description}</div>
    <div class="table-cell">{asset.external_id}</div>
</div>
{/each}
</div>

</div>
</div>
