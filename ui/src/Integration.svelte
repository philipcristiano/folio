<script lang="ts">

  import { onMount } from 'svelte';

  export let id;
  export let provider_name;
  export let accounts = [];
  export let state = "";

  let fiat_total = "";
  let message = "";


  async function getIntegrationAccounts() {
    let params = {integration_id : id};
    let path = "/api/holdings?" + new URLSearchParams(params);
    let response = await fetch(path, {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        accounts = json.holdings;
        fiat_total = json.fiat_total;
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
        <p class="text-lg">
        Provider:


        { provider_name } : ${ fiat_total }
        </p>

        {#if state == "running"}<p class="text-sm">[in progress - refresh in a minute]</p>
        {:else if state == "error"} <p class="text-lg"> [ERROR]</p>
        {:else if state == "complete"}
        {:else} <p class="text-lg">[UNDEFINED]</p>
        {/if}

        {#each accounts as integration_account }
        <p class="text-base">
        { integration_account.symbol }
        { integration_account.asset_balance }
        {#if integration_account.fiat_value }
        $ {integration_account.fiat_value}
        {/if}
        </p>
        {/each}

</div>

