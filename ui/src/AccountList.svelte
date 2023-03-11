<script lang="ts">

  import { onMount } from 'svelte';

  let message = "";
  let address = "";
  let integration_names = [];
  let integration_setups = [];
  let integrations = [];

  async function getIntegrationNames() {
    let response = await fetch("/integrations/add", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        json.integrations.forEach(e => getIntegrationSetup(e));
        integration_names = json.integrations;
    } else {
        message = json.message;
    };
  }
  async function getIntegrationSetup(Name) {
    let response = await fetch("/integrations/add/" + Name, {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        let setup = {name: Name,
                 input_fields: json.setup_properties,
                 inputs: {}};
        integration_setups = [...integration_setups, setup];
    } else {
        message = json.message;
    };
  }
  async function setupIntegration(integration) {

      let response = await fetch("/integrations/add/" + integration.name, {
          method: "POST",
          headers: {
              'Content-Type': 'application/json'
          },
          body: JSON.stringify(integration.inputs),
      })
      let json = await response.json()
      if (response.ok) {
          message = "account added: " + Name
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
      getIntegrationNames();
      getIntegrations();
  });

</script>

<div class="justify-center" >
    {#if message}
        <div><h2> {message} </h2></div>
    {/if}

    <div class="columns-1">
    Available integration providers:
    {#each integration_names as addableIntegrationName }
    <div>
        { addableIntegrationName }
    </div>
    {/each}
    </div>

    Add a new integration:
    {#each integration_setups as addableIntegration }
    <div class="columns-2">
        <div> Name: { addableIntegration.name } </div>
        <div>
            {#each addableIntegration.input_fields as field }
            <input bind:value={addableIntegration.inputs[field]} placeholder="{field}">
            {/each}
            <button type="submit" on:click={() => setupIntegration(addableIntegration)}>Add</button>
        </div>
    </div>
    {/each}

    Current installed integrations:
    {#each integrations as integration (integration.id)}
    <div>
        ID: { integration.id }
        Provider: { integration.provider_name }

        {#each integration.accounts as integration_account (integration_account.external_id)}
        Symbol: { integration_account.symbol }
        Balance: { integration_account.balance }
        <button type="submit" on:click={() => syncIntegration(integration)}>Sync</button>
        {/each}
    </div>
    {/each}

</div>

