<script lang="ts">

  import { onMount } from 'svelte';

  export let id;
  export let provider_name;
  export let accounts = [];

  let message = "";


  async function getIntegrationAccounts() {
    let response = await fetch("/integrations/" + id + "/accounts", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        accounts = json.accounts;
    } else {
        message = json.message;
    };
  }

  onMount(() => {
    getIntegrationAccounts()
  });

</script>

    {#if message}
        <div><h2> {message} </h2></div>
    {/if}

<div class="justify-center" >
        Provider: { provider_name }

        {#each accounts as integration_account (integration_account.external_id, integration_account.symbol)}
        <div class="max-w-md">
        Symbol: { integration_account.symbol }
        Balance: { integration_account.balance }
        </div>
        {/each}

</div>

