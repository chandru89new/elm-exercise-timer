const app = Elm.Main.init({
  node: document.getElementById("app")
});

const synth = new Tone.Synth().toDestination();

// setTimeout(async () => {
//   await Tone.start();
//   synth.triggerAttackRelease("C4", "2n");
// }, 3000);

app?.ports?.beep?.subscribe(() => {
  synth.triggerAttackRelease("G3", "8n");
});
app?.ports?.boop?.subscribe(() => {
  synth.triggerAttackRelease("C4", "4n");
});
app?.ports?.buup?.subscribe(() => {
  synth.triggerAttackRelease("C3", "8n");
});
app?.ports?.bip?.subscribe(() => {
  synth.triggerAttackRelease("G4", "32n");
});
