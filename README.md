# opencc.el

## 介绍

`opencc.el` 利用 [OpenCC](https://github.com/BYVoid/OpenCC) 的命令行工具实现在 Emacs 内进行中文简繁转换。

## 需求

- Emacs 版本至少 24.1
- OpenCC

## 使用

### `(opencc-string CONFIG STRING)`

按配置文件 CONFIG 转换字符串 STRING。使用举例：


```el
(opencc-string "s2t" "简繁转换")
     => "簡繁轉換"
```


其中 `s2t` 是指“把简体转换成繁体”，OpenCC 预设的配置有：

| 配置 | 说明 | 说明 |
| --- | --- | --- |
| s2t      | 简体到繁体                                         | Simplified Chinese to Traditional Chinese                                               |
| t2s      | 繁体到简体                                         | Traditional Chinese to Simplified Chinese                                               |
| s2tw     | 简体到台湾正体                                     | Simplified Chinese to Traditional Chinese <Taiwan Standard>                             |
| tw2s     | 台湾正体到简体                                     | Traditional Chinese <Taiwan Standard> to Simplified Chinese                             |
| s2hk     | 简体到香港繁体（香港小学学习字词表标准）           | Simplified Chinese to Traditional Chinese <Hong Kong Standard>                          |
| hk2s     | 香港繁体（香港小学学习字词表标准）到简体           | Traditional Chinese <Hong Kong Standard> to Simplified Chinese                          |
| s2twp    | 简体到繁体（台湾正体标准）并转换为台湾常用词汇     | Simplified Chinese to Traditional Chinese <Taiwan Standard> with Taiwanese idiom        |
| tw2sp    | 繁体（台湾正体标准）到简体并转换为中国大陆常用词汇 | Traditional Chinese <Taiwan Standard> to Simplified Chinese with Mainland Chinese idiom |

### `(opencc-region CONFIG START END)`

按配置文件 CONFIG 转换 START 和 END 之间的文字。

### `M-x opencc-message`

从 minibuffer 读取配置和字符串，然后用 `message` 显示结果。使用举例：

    M-x opencc-message s2t RET 简繁转换 RET

会显示“簡繁轉換”。

### `M-x opencc-replace-at-point`

从 Minibuffer 读取配置，转换并替换选中区域内的中文。如果没有可用的选中区域，用光标下的一句话。

### `M-x opencc-print-buffer`

转化当前 Buffer 的内容，在另一个 Buffer 中显示转换的结果。

### `M-x opencc-insert-mode`

这是一个全局的 Minor Mode，如果启动了它，所输入的简体字会自动转换成繁体字。可通过选项 `opencc-insert-mode-config` 调整转换的方向。
