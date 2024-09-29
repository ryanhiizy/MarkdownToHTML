export type State = Readonly<{
    title: string;
    markdown: string;
    HTML: string;
    renderHTML: boolean;
    save: boolean;
}>;
