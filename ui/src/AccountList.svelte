<script lang="ts">

  import { onMount } from 'svelte';

  export let accounts = [];
  let message = "";
  let address = "";

  async function getAccounts() {
    let response = await fetch("/accounts", {
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
      getAccounts();
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

    {#each accounts as account (account.id)}
    <div>
        { account.symbol }
        { account.balance }
    </div>
    {/each}


    <input bind:value={address} placeholder="enter your bitcoin address">
    <button type="submit" on:click={addAddress}>Add</button>
</div>

