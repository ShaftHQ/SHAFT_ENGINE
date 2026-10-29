"""Live header measure for L28/L36 (optional Selenium + Chrome).

Fetches home + page URLs at HEADER_VIEWPORTS, reads logo/nav boxes and
documentElement.clientWidth, and returns a header_measure viewports map
suitable for check_cms_header_delta / check_livecms.
"""

from __future__ import annotations

from typing import Any
from urllib.parse import urljoin, urlparse

from hosted_cms_gates import HEADER_VIEWPORTS

DEFAULT_LOGO_SEL = ".header-title-logo a, [data-test='header-logo'], header a[href='/']"
DEFAULT_NAV_SEL = "header nav, .header-nav-list, [data-test='header-nav']"


def _require_selenium():
    try:
        from selenium import webdriver
        from selenium.webdriver.chrome.options import Options
        from selenium.webdriver.common.by import By
        from selenium.webdriver.support import expected_conditions as EC
        from selenium.webdriver.support.ui import WebDriverWait
    except ImportError as exc:  # pragma: no cover - env-specific
        raise RuntimeError(
            "livecms URL measure requires selenium + Chrome "
            "(pip install selenium). Or pass a receipt with header_measure."
        ) from exc
    return webdriver, Options, By, WebDriverWait, EC


def _box(driver, by, sel: str) -> dict[str, float]:
    el = driver.find_element(by, sel)
    rect = driver.execute_script(
        "const r = arguments[0].getBoundingClientRect();"
        "return {x:r.x, y:r.y, w:r.width, h:r.height};",
        el,
    )
    return {
        "x": float(rect["x"]),
        "y": float(rect["y"]),
        "w": float(rect["w"]),
        "h": float(rect["h"]),
    }


def _content_width(driver) -> float:
    return float(
        driver.execute_script("return document.documentElement.clientWidth") or 0
    )


def _origin_home(page_url: str, home_url: str | None) -> str:
    if home_url:
        return home_url
    parsed = urlparse(page_url)
    if not parsed.scheme or not parsed.netloc:
        raise ValueError(f"page_url must be absolute http(s): {page_url!r}")
    return f"{parsed.scheme}://{parsed.netloc}/"


def measure_header_viewports(
    page_url: str,
    *,
    home_url: str | None = None,
    widths: tuple[int, ...] = HEADER_VIEWPORTS,
    logo_sel: str = DEFAULT_LOGO_SEL,
    nav_sel: str = DEFAULT_NAV_SEL,
    height: int = 900,
) -> dict[str, Any]:
    """Headless-measure logo/nav vs home at each width; include content widths."""
    webdriver, Options, By, WebDriverWait, EC = _require_selenium()
    home = _origin_home(page_url, home_url)
    options = Options()
    options.add_argument("--headless=new")
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("--force-device-scale-factor=1")
    # Prefer stable gutter when the site opts in; still record clientWidth.
    options.add_argument("--window-size=1440,900")
    driver = webdriver.Chrome(options=options)
    viewports: dict[str, Any] = {}
    try:
        for width in widths:
            driver.set_window_size(int(width), int(height))
            driver.get(home)
            WebDriverWait(driver, 20).until(EC.presence_of_element_located((By.CSS_SELECTOR, "body")))
            home_logo = _box(driver, By.CSS_SELECTOR, logo_sel)
            home_nav = _box(driver, By.CSS_SELECTOR, nav_sel)
            home_w = _content_width(driver)
            driver.get(page_url)
            WebDriverWait(driver, 20).until(EC.presence_of_element_located((By.CSS_SELECTOR, "body")))
            page_logo = _box(driver, By.CSS_SELECTOR, logo_sel)
            page_nav = _box(driver, By.CSS_SELECTOR, nav_sel)
            page_w = _content_width(driver)
            viewports[str(width)] = {
                "home_logo": {"x": home_logo["x"], "y": home_logo["y"]},
                "page_logo": {"x": page_logo["x"], "y": page_logo["y"]},
                "home_nav": {"x": home_nav["x"], "y": home_nav["y"]},
                "page_nav": {"x": page_nav["x"], "y": page_nav["y"]},
                "home_content_width": home_w,
                "page_content_width": page_w,
            }
    finally:
        driver.quit()
    return {
        "page_url": page_url,
        "home_url": home,
        "viewports": viewports,
        "logo_sel": logo_sel,
        "nav_sel": nav_sel,
    }


def looks_like_url(value: str) -> bool:
    parsed = urlparse(value or "")
    return parsed.scheme in {"http", "https"} and bool(parsed.netloc)
