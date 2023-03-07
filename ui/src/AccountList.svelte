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
  async function addAddress(e) {
      const data = {};
      data["address"] = address;
      let response = await fetch("/accounts/bitcoin", {
          method: "POST",
          headers: {
              'Content-Type': 'application/json'
          },
          body: JSON.stringify(data),
      })
      let json = await response.json()
      if (response.ok) {
          message = "account added"
      } else {
          message = json.message;
      };
  }

</script>

<div class="columns-2">
    {#if message}
    <h2> {message} </h2>
    {/if}

    {#each integration_names as addableIntegrationName }
    <div>
        { addableIntegrationName }
    </div>
    {/each}

    {#each integration_setups as addableIntegration }
    <div>
        Name: { addableIntegration.name }
            {#each addableIntegration.input_fields as field }
            <input bind:value={addableIntegration.inputs[field]} placeholder="{field}">
            {/each}
            <button type="submit" on:click={() => setupIntegration(addableIntegration)}>Add</button>
    </div>
    {/each}

    {#each integrations as integration (integration.id)}
    <div>
        ID: { integration.id }
        Provider: { integration.provider_name }
        Symbol: { integration.symbol }
        Balance: { integration.balance }
    </div>
    {/each}


    <input bind:value={address} placeholder="enter your bitcoin address">
    <button type="submit" on:click={addAddress}>Add</button>
</div>

