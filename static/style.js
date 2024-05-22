document.getElementById("commentEditButton").addEventListener('click',() => {
  let form = document.getElementById('commentEditForm');
  
  if(form.style.display == 'none' || form.style.display == '')
    form.style.display = 'block';
  else
    form.style.display = 'None';
})
