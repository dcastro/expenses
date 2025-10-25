import Choices from 'choices.js';

export const init = elementId => () => {
  // NOTE: Choices.js will automatically hide the original select element, and add a new one to the DOM.
  // However, when the Halogen component is re-rendered, it'll be shown again.
  //
  // Workaround: manually hide the original element.
  document.getElementById(elementId).style.display = 'none';

  let choices = new Choices(`#${elementId}`, {
    removeItems: true,
    removeItemButton: true,
    placeholder: true,
    placeholderValue: "Search...",
    itemSelectText: '',

    // From the docs:
    // > By default, if there is not enough space within the window the dropdown will appear above the input, otherwise below it.
    //
    // On mobile devices, there might not be enough space below, within the viewport, causing the dropdown to be displayed above.
    // However, our table has `overflow-y: hidden` to make it scrollable,
    // which means the dropdown will be clipped if it tries to escape the boundaries of the table.
    //
    // To prevent this from happening, we'll always show the dropdown below the input, where
    // there is always enough space to display it.
    position: "bottom",

    // Disable sorting so that "No tag" appears at the top
    shouldSort: false,
  });

  // Set a global var so we can manually interact with the chart in the browser's js console
  // Accessible via e.g.:
  //   `globalThis["tag-choices"]`
  //   `globalThis["account-choices"]`
  globalThis[elementId] = choices;

  return choices;
}


export const selectValue = choices => value => () => {
  console.log("Setting choices value to", value);
  choices.setChoiceByValue(value);
}

export const clearSelection = choices => () => {
  console.log("Clearing selection")
  choices.removeActiveItems();
}
