
// Function for front and back covers, footer and logo images

(function() {
  // Retrieve previous config object if defined
  window.PagedConfig = window.PagedConfig || {};
  const {before: beforePaged} = window.PagedConfig;

  // utils
  const insertCSS = text => {
    let style = document.createElement('style');
    style.type = 'text/css';
    style.appendChild(document.createTextNode(text));
    document.head.appendChild(style);
  };

  // Util function for defining CSS image variables
  const insertCSSImgVar = type => {
    const links = document.querySelectorAll('link[id^=' + type + ']');
    if (!links.length) return;
    const re = new RegExp(type + '-\\d+');
    let text = ':root {--' + type + ': var(--' + type + '-1);';
    for (const link of links) {
      text += '--' + re.exec(link.id)[0] + ': url("' + link.href + '");';
    }
    text += '}';
    insertCSS(text);
  };

  // Support for front and back covers, footer and logo images
  window.PagedConfig.before = async () => {
    let frontCover = document.querySelector('.cfa-front-cover');
    let backCover = document.querySelector('.cfa-back-cover');
    let pgFooter = document.querySelector('.cfa-footer');
    let simpleLogo = document.querySelector('.cfa-logo');
    let longLogo = document.querySelector('.cfa-long-logo');
    if (frontCover) document.body.prepend(frontCover);
    if (backCover) document.body.append(backCover);
    if (pgFooter) document.body.prepend(pgFooter);
    if (simpleLogo) document.body.append(simpleLogo);
    if (longLogo) document.body.append(longLogo);
    insertCSSImgVar('cfa-front-cover');
    insertCSSImgVar('cfa-back-cover');
    insertCSSImgVar('cfa-footer');
    insertCSSImgVar('cfa-logo');
    insertCSSImgVar('cfa-long-logo');

    if (beforePaged) await beforePaged();
  };
})();
