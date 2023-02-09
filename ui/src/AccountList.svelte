<script lang="ts">

  import { onMount } from 'svelte';

  export let accounts = [];
  let message = "";

  async function getAccounts() {
    let response = await fetch("/accounts", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        accounts = json.accounts;
        message = "Refresh page: TODO: don't require refresh";
    } else {
        message = json.message;
    };
  }

  onMount(() => {
      getAccounts();
  });

</script>

<div class="columns-2">
    {#if message}
    <h2> {message} </h2>
    {/if}

    {#each accounts as account (account.id)}
    <div>
        { account.symbol }
        { account.balance }
    </div>
    {/each}

</div>

