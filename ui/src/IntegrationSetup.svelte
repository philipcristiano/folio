<script lang="ts">

  import { onMount } from 'svelte';
  import Button from './Button.svelte';

  let message = "";

  let integration_names = [];
  let selected_add_integration = "";
  let integration_setups = [];
  let accounts = [];

  async function getIntegrationNames() {
    let response = await fetch("/api/integration/add", {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integration_names = json.integrations;
    } else {
        message = json.message;
    };
  }
  async function getIntegrationSetup(Name) {
    let response = await fetch("/api/integration/add/" + Name, {
        method: "GET",
    });
    let json = await response.json()
    if (response.ok) {
        integration_setups = [];
        json.setup_properties.forEach(SP =>

          integration_setups = [...integration_setups,
                  {name: Name,
                   input_fields: SP,
                   inputs: {}}

                  ]

        )
    } else {
        message = json.message;
    };
  }
  async function setupIntegration(integration) {

      let response = await fetch("/api/integration/add/" + integration.name, {
          method: "POST",
          headers: {
              'Content-Type': 'application/json'
          },
          body: JSON.stringify(integration.inputs),
      })
      let json = await response.json()
      if (response.ok) {
          message = "account added: " + integration.Name
      } else {
          message = json.message;
      };
  }

  onMount(() => {
    getIntegrationNames();
  });

</script>

<div class="border-grey max-w-sm border-2">
<div class="max-w-sm">

{#if message}
    <div><h2> {message} </h2></div>
{/if}

Add Integration:
    <select bind:value={selected_add_integration} on:change="{() => getIntegrationSetup(selected_add_integration)}">
        {#each integration_names as addableIntegrationName }
			<option value={addableIntegrationName}>
				{addableIntegrationName}
			</option>
		{/each}
	</select>
</div>

{#each integration_setups as addableIntegration }
<div class="border-black">
    <div> Name: { addableIntegration.name } </div>
    <div>{#each addableIntegration.input_fields as field }<div>

          {#if field.type == "text" }
          <input bind:value={addableIntegration.inputs[field.name]} placeholder="{field.name}">
          {:else if field.type == "choice"}
          {#each field.choices as field_choice}
          <label>
	          <input class="p-1" type=radio bind:group={addableIntegration.inputs[field.name]} name="{field_choice}" value={field_choice}>
            {field_choice}
            </label>
          {/each}
          {:else }
          Unknown input type {field.type}
          {/if}

    </div>{/each}</div>
        <div> <Button on:click={() => setupIntegration(addableIntegration)}>Add</Button></div>
</div>
<hr>
{/each}
</div>
