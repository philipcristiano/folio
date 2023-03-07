<script lang="ts">

  import { onMount } from 'svelte';

  let message = "";
  let address = "";
  let integration_names = [];
  let integration_setups = [];
  let integrations = [];

  async function getIntegrationNames() {
    let response = await fetch("/accounts/add", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integration_names = json.integrations;
        integration_names.forEach(e => getIntegrationSetup(e));
    } else {
        message = json.message;
    };
  }
  async function getIntegrationSetup(Name) {
    let response = await fetch("/accounts/add/" + Name, {
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

      let response = await fetch("/accounts/add/" + integration.name, {
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
    let response = await fetch("/accounts", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integrations = json.integrations;
    } else {
        message = json.message;
    };
  }

  onMount(() => {
      getIntegrationNames();
      getIntegrations();
  });

</script>

<div class="columns-2">
    {#if message}
    <h2> {message} </h2>
    {/if}

    Available integration providers:
    {#each integration_names as addableIntegrationName }
    <div>
        { addableIntegrationName }
    </div>
    {/each}

    Add a new integration:
    {#each integration_setups as addableIntegration }
    <div>
        Name: { addableIntegration.name }
            {#each addableIntegration.input_fields as field }
            <input bind:value={addableIntegration.inputs[field]} placeholder="{field}">
            {/each}
            <button type="submit" on:click={() => setupIntegration(addableIntegration)}>Add</button>
    </div>
    {/each}

    Current installed integrations:
    {#each integrations as integration (integration.id)}
    <div>
        ID: { integration.id }
        Provider: { integration.provider_name }
        Symbol: { integration.symbol }
        Balance: { integration.balance }
    </div>
    {/each}

</div>

