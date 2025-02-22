console.log("custom js loaded!");

function updateCharCount(textarea) {
    const maxlength = 250;
    const currentLen = textarea.value.length;
    const remainingChars = maxlength - currentLen;
    const charCountEle = document.getElementById('charCount');
    const charCountDiv = document.getElementById('charCountDiv');
    charCountEle.textContent = remainingChars;
    if (remainingChars < 50) {
      charCountDiv.classList.remove('text-yellow-500');
      charCountDiv.classList.add('text-red-600');
    }
    else if (remainingChars < 100) {
      charCountDiv.classList.remove('text-green-600');
      charCountDiv.classList.add('text-yellow-600');
    } 
}
