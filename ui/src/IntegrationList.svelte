<script lang="ts">

  import { onMount } from 'svelte';

  import Integration from './Integration.svelte';

  let message = "";
  let address = "";
  let integration_names = [];
  let integration_setups = [];
  let integrations = [];
  let transactions = [];

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
      getIntegrationNames();
      getIntegrations();
      getTransactions();
  });

</script>

<div class="columns-6">
    <div class="max-w-md">
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
            <Integration {...integration} />
            <button type="submit" on:click={() => syncIntegration(integration)}>Sync</button>
        </div>
        {/each}


    </div>

    <div class="max-w-md">
        Transactions:
        <table class="w-full border-collapse bg-white text-left text-sm text-gray-500">
        <thead class="bg-gray-50">
            <tr>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Datetime</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Symbol</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Direction</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Amount</th>
              <th scope="col" class="px-6 py-4 font-medium text-gray-900">Description</th>
            </tr>
        </thead>
        <tbody>
        {#each transactions as tx}
        <tr>
            <td>{tx.timestamp}</td>
            <td>{tx.symbol}</td>
            <td>{tx.direction}</td>
            <td>{tx.amount}</td>
            <td>{tx.description}</td>
        </tr>
        {/each}

        </table>

    </div>

</div>
